;;; nyx-org-claude.el --- Query/write bridge for Claude Code <-> Org -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; A bridge that lets Claude Code collaborate on this Org-mode GTD system by
;; talking to the *running* Emacs via `emacsclient'.  Because it runs inside
;; the live Emacs, every query and edit uses the live config (todo keywords,
;; tag inheritance, `org-agenda-files') and the live buffers, including unsaved
;; changes.  The companion shell wrapper (`orgq', shipped with the Claude
;; "org" skill) calls these functions and exchanges JSON.
;;
;; Conventions:
;; - Reads return JSON; writes return the affected entry as JSON.
;; - Writes target either {"id":"..."} or {"file":"rel/abs.org","path":[...]};
;;   the outline path is matched exactly with `org-find-olp'.
;; - Safety model: single-entry writes auto-save (`nyx-org-claude--save-current').
;;   Bulk/destructive ops are expected to gate behind a diff preview + confirm
;;   and to suppress auto-save; `raw' callers manage their own saving.
;;
;;; Code:

(require 'org)
(require 'org-ql)
(require 'json)
(require 'org-id)
(require 'org-archive)
(require 'org-attach)
(require 'cl-lib)
(require 'subr-x)
(require 'nyx-org-agenda)          ; for the shared `nyx-stuck-projects-query'

;;;; ---- helpers ---------------------------------------------------------

(defun nyx-org-claude--s (x)
  "Strip text properties from string X (or return \"\")."
  (if (stringp x) (substring-no-properties x) ""))

(defun nyx-org-claude--entry-alist ()
  "Return the Org entry at point as a JSON-ready alist."
  (let ((tags (mapcar #'nyx-org-claude--s (org-get-tags)))
        (olp  (mapcar #'nyx-org-claude--s (org-get-outline-path))))
    (list (cons "heading"   (nyx-org-claude--s (org-get-heading t t t t)))
          (cons "state"     (nyx-org-claude--s (org-get-todo-state)))
          (cons "priority"  (let ((p (nth 3 (org-heading-components))))
                              (if p (char-to-string p) "")))
          (cons "tags"      (apply #'vector tags))
          (cons "scheduled" (nyx-org-claude--s (org-entry-get nil "SCHEDULED")))
          (cons "deadline"  (nyx-org-claude--s (org-entry-get nil "DEADLINE")))
          (cons "id"        (nyx-org-claude--s (org-entry-get nil "ID")))
          (cons "file"      (nyx-org-claude--s (buffer-file-name)))
          (cons "olp"       (apply #'vector olp))
          (cons "path"      (apply #'vector (append olp
                                                    (list (nyx-org-claude--s
                                                           (org-get-heading t t t t))))))
          (cons "level"     (or (org-current-level) 0)))))

(defun nyx-org-claude--entry-json ()
  "Return the Org entry at point encoded as JSON."
  (json-encode (nyx-org-claude--entry-alist)))

(defun nyx-org-claude--target (target)
  "Resolve TARGET (a JSON string or alist) to a marker.
TARGET is {\"id\":...} or {\"file\":...,\"path\":[...]}."
  (let* ((tgt (if (stringp target)
                  (json-parse-string target :object-type 'alist :array-type 'list)
                target))
         (id   (alist-get 'id tgt))
         (file (alist-get 'file tgt))
         (path (alist-get 'path tgt)))
    (cond
     ((and id (stringp id) (not (string-empty-p id)))
      (or (org-id-find id 'marker)
          (car (org-ql-select (org-agenda-files) `(property "ID" ,id)
                 :action '(point-marker)))
          (error "nyx-org-claude: no entry with ID %s" id)))
     ((and file path)
      (let ((abs (if (file-name-absolute-p file) file
                   (expand-file-name file org-directory))))
        (or (org-find-olp (cons abs (append path nil)))
            (error "nyx-org-claude: no entry at %s :: %S" file path))))
     (t (error "nyx-org-claude: target needs \"id\" or \"file\"+\"path\"")))))

(defvar nyx-org-claude--no-save nil
  "When non-nil, `nyx-org-claude--save-current' is a no-op.
Bound by bulk staging so edits accumulate unsaved for a diff preview.")

(defun nyx-org-claude--save-current ()
  "Save the current buffer when it visits a file (unless staging).
Single-entry writes auto-save (per the agreed safety model).  Bulk and raw
operations suppress this via `nyx-org-claude--no-save' and gate on a preview."
  (when (and (not nyx-org-claude--no-save) (buffer-file-name)) (save-buffer)))

(defmacro nyx-org-claude--at-target (target &rest body)
  "Run BODY with point at TARGET's entry, auto-save, return the entry JSON."
  (declare (indent 1))
  `(org-with-point-at (nyx-org-claude--target ,target)
     (org-back-to-heading t)
     ,@body
     (nyx-org-claude--save-current)
     (nyx-org-claude--entry-json)))

;;;; ---- read ------------------------------------------------------------

(defun nyx-org-claude-query (query)
  "Run org-ql QUERY across the agenda files, return a JSON array of entries."
  (json-encode
   (apply #'vector
          (org-ql-select (org-agenda-files) query
            :action #'nyx-org-claude--entry-alist))))

(defun nyx-org-claude-stuck-projects ()
  "Return JSON for stuck projects, using the shared `nyx-stuck-projects-query'.
Matches the agenda's \"Stuck Projects\" block exactly."
  (json-encode
   (apply #'vector
          (org-ql-select (org-agenda-files) nyx-stuck-projects-query
            :action #'nyx-org-claude--entry-alist))))

(defvar nyx-org-claude-views
  '(("next-actions"  . nyx-next-actions-query)
    ("waiting"       . nyx-waiting-query)
    ("high-priority" . nyx-high-priority-query)
    ("inbox"         . nyx-inbox-query)
    ("stuck"         . nyx-stuck-projects-query))
  "Named org-ql views shared with `org-agenda-custom-commands'.
Each value is a query-holding defvar from `nyx-org-agenda'.")

(defun nyx-org-claude-views-list ()
  "Return JSON array of available view names."
  (json-encode (apply #'vector (mapcar #'car nyx-org-claude-views))))

(defun nyx-org-claude-view (name)
  "Run the named view NAME (see `nyx-org-claude-views') and return JSON entries."
  (let ((sym (or (cdr (assoc name nyx-org-claude-views))
                 (error "nyx-org-claude: unknown view %s (have: %s)" name
                        (mapconcat #'car nyx-org-claude-views ", ")))))
    (json-encode
     (apply #'vector
            (org-ql-select (org-agenda-files) (symbol-value sym)
              :action #'nyx-org-claude--entry-alist)))))

(defvar nyx-org-claude-inbox-buckets '("Email" "Tasks" "Projects" "Notes")
  "Level-1 container headings in inbox.org that are not themselves triage items.
These inherit :inbox: via the file's #+FILETAGS and must be skipped by
`nyx-org-claude-inbox'.")

(defun nyx-org-claude--entry-body (&optional max)
  "Return the trimmed body text of the entry at point (point on heading).
Caps at MAX chars (default 500), appending an ellipsis when truncated."
  (let ((end (save-excursion (outline-next-heading) (point)))
        (beg (save-excursion (org-end-of-meta-data t) (point)))
        (cap (or max 500)))
    (if (>= beg end) ""
      (let ((raw (string-trim (buffer-substring-no-properties beg end))))
        (if (> (length raw) cap) (concat (substring raw 0 cap) " …") raw)))))

(defun nyx-org-claude-inbox ()
  "Return JSON array of :inbox:-tagged items for triage, each with a body snippet.
Excludes the structural bucket containers in inbox.org (see
`nyx-org-claude-inbox-buckets')."
  (let ((inbox-file (expand-file-name "inbox.org" org-directory)))
    (json-encode
     (apply #'vector
            (delq nil
                  (org-ql-select (org-agenda-files) '(tags "inbox")
                    :action
                    (lambda ()
                      (unless (and (buffer-file-name)
                                   (string= (expand-file-name (buffer-file-name)) inbox-file)
                                   (equal (org-current-level) 1)
                                   ;; strip any [n/m]/[n%] statistics cookie before matching
                                   (member (string-trim
                                            (replace-regexp-in-string
                                             "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" ""
                                             (org-get-heading t t t t)))
                                           nyx-org-claude-inbox-buckets))
                        (append (nyx-org-claude--entry-alist)
                                (list (cons "body" (nyx-org-claude--entry-body))))))))))))

(defun nyx-org-claude-get (target)
  "Return TARGET's entry as JSON, including its full (uncapped) body text.
Like a single-entry `query' result with an added \"body\" field.  Use this to
read a task's notes/content through the bridge instead of opening the raw
.org file.  Body is the text between the entry's metadata and its first child
heading (property drawer and planning lines excluded)."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (json-encode
     (append (nyx-org-claude--entry-alist)
             (list (cons "body" (nyx-org-claude--entry-body most-positive-fixnum)))))))

;;;; ---- write (single-entry; auto-saves) -------------------------------

(defun nyx-org-claude-state (target state)
  "Set TODO STATE on TARGET."
  (nyx-org-claude--at-target target (org-todo state)))

(defun nyx-org-claude--read-date (s)
  "Normalize date string S via `org-read-date' (accepts +2w, \"fri\", ISO, ...)."
  (org-read-date nil nil s))

(defun nyx-org-claude-schedule (target date)
  "Set SCHEDULED on TARGET to DATE, or clear it when DATE is empty/\"-\".
DATE may be any `org-read-date' expression (e.g. +2w, \"next fri\", 2026-06-15)."
  (nyx-org-claude--at-target target
    (if (or (null date) (member date '("" "-")))
        (org-schedule '(4))
      (org-schedule nil (nyx-org-claude--read-date date)))))

(defun nyx-org-claude-deadline (target date)
  "Set DEADLINE on TARGET to DATE, or clear it when DATE is empty/\"-\".
DATE may be any `org-read-date' expression."
  (nyx-org-claude--at-target target
    (if (or (null date) (member date '("" "-")))
        (org-deadline '(4))
      (org-deadline nil (nyx-org-claude--read-date date)))))

(defun nyx-org-claude-priority (target priority)
  "Set PRIORITY (\"A\"/\"B\"/\"C\") on TARGET, or remove it when empty/\"-\"."
  (nyx-org-claude--at-target target
    (if (or (null priority) (member priority '("" "-")))
        (org-priority ?\s)
      (org-priority (string-to-char (upcase priority))))))

(defun nyx-org-claude-tags (target op tags)
  "Edit local tags on TARGET.  OP is add/remove/set; TAGS is space/comma separated."
  (nyx-org-claude--at-target target
    (let ((lst (split-string (or tags "") "[ ,]+" t))
          (cur (org-get-tags nil t)))      ; local tags only, no inheritance
      (pcase op
        ("set"    (org-set-tags lst))
        ("add"    (org-set-tags (delete-dups (append cur lst))))
        ("remove" (org-set-tags (seq-difference cur lst)))
        (_ (error "nyx-org-claude: tag op must be add/remove/set"))))))

(defun nyx-org-claude-ensure-id (target)
  "Ensure the entry at TARGET has an ID; return the entry (with id populated)."
  (nyx-org-claude--at-target target
    (org-id-get-create)))

(defun nyx-org-claude-refile-targets (&optional file match)
  "Return JSON array of valid refile targets {name, file}.
With FILE, restrict to that file (relative to `org-directory' or absolute).
With MATCH, keep only targets whose heading name contains MATCH (case-insensitive)
— useful to narrow the (often large) target set during triage."
  (let* ((org-refile-targets
          (if (and file (not (string-empty-p file)))
              (let ((abs (if (file-name-absolute-p file) file
                           (expand-file-name file org-directory))))
                `(((,abs) :maxlevel . 3)))
            org-refile-targets))
         (targets (org-refile-get-targets)))
    (when (and match (not (string-empty-p match)))
      (let ((needle (downcase match)))
        (setq targets (seq-filter
                       (lambda (tgt) (string-search needle (downcase (or (nth 0 tgt) ""))))
                       targets))))
    (json-encode
     (apply #'vector
            (mapcar (lambda (tgt)
                      (list (cons "name" (nyx-org-claude--s (nth 0 tgt)))
                            (cons "file" (nyx-org-claude--s (nth 1 tgt)))))
                    targets)))))

(defun nyx-org-claude-prop (target name value)
  "Set property NAME=VALUE on TARGET (empty VALUE deletes it)."
  (nyx-org-claude--at-target target
    (if (or (null value) (string-empty-p value))
        (org-delete-property name)
      (org-set-property name value))))

(defun nyx-org-claude-body (target mode text)
  "Edit TARGET's body.  MODE is \"append\" or \"replace\"."
  (nyx-org-claude--at-target target
    (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
          (end (save-excursion (outline-next-heading) (point))))
      (if (string= mode "replace")
          (progn (delete-region beg end)
                 (goto-char beg)
                 (insert (string-trim-right text) "\n"))
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (insert (string-trim-right text) "\n")))))

(defun nyx-org-claude-refile (target dest)
  "Refile entry at TARGET under DEST ({file,path} of an existing heading)."
  (let* ((dtgt (if (stringp dest)
                   (json-parse-string dest :object-type 'alist :array-type 'list)
                 dest))
         (dfile (let ((f (alist-get 'file dtgt)))
                  (if (file-name-absolute-p f) f (expand-file-name f org-directory))))
         (dpath (append (alist-get 'path dtgt) nil))
         (dmark (org-find-olp (cons dfile dpath))))
    (org-with-point-at (nyx-org-claude--target target)
      (org-back-to-heading t)
      (let ((src (current-buffer)))
        (org-refile nil nil (list (car (last dpath)) dfile nil
                                  (marker-position dmark)))
        ;; refile touches two files; save both (single move, still "simple")
        (with-current-buffer (marker-buffer dmark) (nyx-org-claude--save-current))
        (with-current-buffer src (nyx-org-claude--save-current)))
      (json-encode `(("refiled" . t)
                     ("to_file" . ,dfile)
                     ("to_path" . ,(apply #'vector dpath)))))))

(defun nyx-org-claude-add-child (target state title)
  "Append a child heading under TARGET with TODO STATE and TITLE.
STATE may be empty for a plain heading.  Child level = parent + 1."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (let ((stars (make-string (1+ (org-current-level)) ?*))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (let ((hp (point)))
        (insert stars " "
                (if (or (null state) (string-empty-p state)) "" (concat state " "))
                title "\n")
        (goto-char hp)))          ; land on the inserted heading, not the next one
    (org-back-to-heading t)
    (nyx-org-claude--save-current)
    (nyx-org-claude--entry-json)))

(defun nyx-org-claude-move (target direction &optional n)
  "Reorder the heading at TARGET among its siblings.
DIRECTION is \"up\", \"down\", \"top\" or \"bottom\"; N steps (default 1)."
  (nyx-org-claude--at-target target
    (let ((n (or n 1)))
      (pcase direction
        ("up"     (dotimes (_ n) (org-move-subtree-up)))
        ("down"   (dotimes (_ n) (org-move-subtree-down)))
        ("top"    (ignore-errors (while t (org-move-subtree-up))))
        ("bottom" (ignore-errors (while t (org-move-subtree-down))))
        (_ (error "nyx-org-claude: direction must be up/down/top/bottom"))))))

(defun nyx-org-claude-sort (target key &optional reverse)
  "Sort the children of TARGET by KEY.
KEY is priority/todo/alpha/numeric/scheduled/deadline; REVERSE descends."
  (nyx-org-claude--at-target target
    (let ((type (pcase key
                  ("priority"  ?p) ("todo"      ?o) ("alpha"   ?a)
                  ("numeric"   ?n) ("scheduled" ?s) ("deadline" ?d)
                  (_ (error "nyx-org-claude: sort key must be priority/todo/alpha/numeric/scheduled/deadline")))))
      (org-sort-entries nil (if reverse (upcase type) type)))))

(defun nyx-org-claude-new-project (file title &optional under)
  "Create a :project: heading TITLE in FILE under heading UNDER (default \"Projects\").
Adds a Tasks subheading with a fresh ID.  Returns the project entry."
  (let* ((abs (if (file-name-absolute-p file) file (expand-file-name file org-directory)))
         (parent (if (and under (not (string-empty-p under))) under "Projects")))
    (with-current-buffer (find-file-noselect abs)
      (let ((pmark (or (org-find-olp (list abs parent))
                       (error "nyx-org-claude: no \"%s\" heading in %s" parent file))))
        (org-with-point-at pmark
          (let ((lvl (org-current-level)))
            (goto-char (save-excursion (org-end-of-subtree t t) (point)))
            (unless (bolp) (insert "\n"))
            (insert (make-string (1+ lvl) ?*) " " title "\n")
            (forward-line -1) (org-back-to-heading t)
            (org-set-tags '("project"))
            (goto-char (save-excursion (org-end-of-subtree t t) (point)))
            (unless (bolp) (insert "\n"))
            (insert (make-string (+ 2 lvl) ?*) " Tasks\n")
            (forward-line -1) (org-back-to-heading t)
            (org-id-get-create))))
      (nyx-org-claude--save-current)
      (org-with-point-at (org-find-olp (list abs parent title))
        (nyx-org-claude--entry-json)))))

(defun nyx-org-claude-capture (text)
  "Append raw Org TEXT to the end of inbox.org."
  (with-current-buffer (find-file-noselect
                        (expand-file-name "inbox.org" org-directory))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (string-trim-right text) "\n")
    (nyx-org-claude--save-current)
    (json-encode `(("captured" . t)
                   ("file" . ,(buffer-file-name))))))

;;;; ---- safety: diff / revert -------------------------------------------

(defun nyx-org-claude-diff (&optional file)
  "Return JSON array of {file, diff} for modified Org buffers.
Each diff is a unified diff of the on-disk (saved) file vs the live buffer.
With FILE, restrict to that file (relative to `org-directory' or absolute)."
  (let ((want (and file (not (string-empty-p file))
                   (expand-file-name (if (file-name-absolute-p file) file
                                       (expand-file-name file org-directory)))))
        results)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and (derived-mode-p 'org-mode) (buffer-modified-p) (buffer-file-name)
                   (or (not want) (string= want (expand-file-name (buffer-file-name)))))
          (let ((bf (buffer-file-name))
                (tmp (make-temp-file "orgq-diff")))
            (write-region (point-min) (point-max) tmp nil 'quiet)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat bf " (saved)")
                            "--label" (concat bf " (buffer)")
                            bf tmp)
              (push (list (cons "file" (nyx-org-claude--s bf))
                          (cons "diff" (buffer-string)))
                    results))
            (delete-file tmp)))))
    (json-encode (apply #'vector (nreverse results)))))

(defun nyx-org-claude-revert (file)
  "Discard unsaved changes in FILE's buffer (relative to `org-directory')."
  (let* ((abs (if (file-name-absolute-p file) file (expand-file-name file org-directory)))
         (buf (find-buffer-visiting abs)))
    (if buf
        (progn (with-current-buffer buf (revert-buffer t t t))
               (json-encode `(("reverted" . ,(nyx-org-claude--s abs)))))
      (json-encode '(("reverted" . :null))))))

;;;; ---- bulk (staged: no auto-save; review with diff, then save/revert) -

(defun nyx-org-claude--apply-op (op args)
  "Apply OP (a string) with ARGS (a list) to the Org entry at point.
Shares semantics with the single-entry ops but does not save."
  (pcase op
    ("state"    (org-todo (nth 0 args)))
    ("schedule" (let ((d (nth 0 args)))
                  (if (member d '("" "-" nil)) (org-schedule '(4))
                    (org-schedule nil (nyx-org-claude--read-date d)))))
    ("deadline" (let ((d (nth 0 args)))
                  (if (member d '("" "-" nil)) (org-deadline '(4))
                    (org-deadline nil (nyx-org-claude--read-date d)))))
    ("priority" (let ((p (nth 0 args)))
                  (if (member p '("" "-" nil)) (org-priority ?\s)
                    (org-priority (string-to-char (upcase p))))))
    ("tags"     (let ((o (nth 0 args))
                      (tg (split-string (or (nth 1 args) "") "[ ,]+" t))
                      (cur (org-get-tags nil t)))
                  (pcase o
                    ("set"    (org-set-tags tg))
                    ("add"    (org-set-tags (delete-dups (append cur tg))))
                    ("remove" (org-set-tags (seq-difference cur tg)))
                    (_ (error "nyx-org-claude: tag op must be add/remove/set")))))
    ("prop"     (let ((n (nth 0 args)) (v (nth 1 args)))
                  (if (member v '("" nil)) (org-delete-property n)
                    (org-set-property n v))))
    (_ (error "nyx-org-claude: unsupported bulk op %s" op))))

(defun nyx-org-claude-bulk (query op &rest args)
  "Apply OP (with ARGS) to every entry matching org-ql QUERY, WITHOUT saving.
OP is one of state/schedule/deadline/priority/tags/prop.  Edits are left
unsaved for a `nyx-org-claude-diff' preview; commit with `nyx-org-claude-save'
or discard with `nyx-org-claude-revert'."
  (let ((nyx-org-claude--no-save t)
        (markers (org-ql-select (org-agenda-files) query :action '(point-marker)))
        (files (make-hash-table :test 'equal))
        (n 0))
    (dolist (m markers)
      (org-with-point-at m
        (org-back-to-heading t)
        (nyx-org-claude--apply-op op args)
        (when (buffer-file-name) (puthash (buffer-file-name) t files))
        (cl-incf n)))
    (json-encode
     (list (cons "matched" n)
           (cons "op" op)
           (cons "staged" t)
           (cons "files" (apply #'vector (hash-table-keys files)))
           (cons "next" "preview with `orgq diff`, then `orgq save` to commit or `orgq revert FILE` to discard")))))

;;;; ---- archiving --------------------------------------------------------

(defun nyx-org-claude-archive (target)
  "Archive the subtree at TARGET to `org-archive-location'.  Destructive; saves."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (let ((src (current-buffer)))
      (org-archive-subtree)
      (with-current-buffer src (when (buffer-file-name) (save-buffer))))
    (json-encode '(("archived" . t)))))

(defun nyx-org-claude-archive-sweep (query)
  "Archive every entry matching org-ql QUERY (destructive).  Saves affected buffers.
Run a plain `nyx-org-claude-query' with the same QUERY first to preview matches."
  (let ((markers (org-ql-select (org-agenda-files) query :action '(point-marker)))
        (bufs (make-hash-table :test 'eq))
        (n 0))
    ;; Reverse document order so removing a subtree doesn't invalidate
    ;; the markers we have yet to process in the same buffer.
    (dolist (m (nreverse markers))
      (org-with-point-at m
        (org-back-to-heading t)
        (puthash (current-buffer) t bufs)
        (org-archive-subtree)
        (cl-incf n)))
    (dolist (b (hash-table-keys bufs))
      (with-current-buffer b (when (buffer-file-name) (save-buffer))))
    (json-encode `(("archived" . ,n)))))

;;;; ---- attachments (org-attach; ID-keyed) ------------------------------

(defun nyx-org-claude--attach-alist ()
  "Return the attachment state of the entry at point as a JSON-ready alist.
Point must be on a heading."
  (let* ((id  (org-entry-get nil "ID"))
         (dir (org-attach-dir))               ; nil if no attach dir yet
         (files (and dir (file-directory-p dir) (org-attach-file-list dir))))
    (list (cons "id"    (nyx-org-claude--s id))
          (cons "dir"   (nyx-org-claude--s dir))   ; "" when no attach dir yet
          (cons "files" (apply #'vector (mapcar #'nyx-org-claude--s files))))))

(defun nyx-org-claude-attach-list (target)
  "Return JSON {id, dir, files} of attachments on TARGET (no changes)."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (json-encode (nyx-org-claude--attach-alist))))

(defun nyx-org-claude-attach-add (target file &optional method)
  "Attach FILE to TARGET via org-attach.  Auto-saves.
METHOD is \"cp\" (default), \"mv\", \"ln\" (hard link) or \"lns\" (symlink).
Creates the entry's ID and attachment dir if needed.  FILE is expanded
against the working directory."
  (let ((src (expand-file-name file))
        (meth (pcase method
                ("mv" 'mv) ("ln" 'ln) ("lns" 'lns)
                ((or "cp" "" `nil) 'cp)
                (_ (error "nyx-org-claude: attach method must be cp/mv/ln/lns")))))
    (unless (file-exists-p src)
      (error "nyx-org-claude: no such file %s" src))
    (org-with-point-at (nyx-org-claude--target target)
      (org-back-to-heading t)
      (org-attach-attach src nil meth)
      (nyx-org-claude--save-current)
      (json-encode
       (cons (cons "attached" (nyx-org-claude--s (file-name-nondirectory src)))
             (nyx-org-claude--attach-alist))))))

(defun nyx-org-claude-attach-path (target name)
  "Return JSON {path} of attachment NAME on TARGET (for retrieval/reading).
Errors if the entry has no attachment dir or no such file."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (let* ((dir (or (org-attach-dir)
                    (error "nyx-org-claude: %s has no attachments"
                           (org-get-heading t t t t))))
           (path (expand-file-name name dir)))
      (unless (file-exists-p path)
        (error "nyx-org-claude: no attachment %s in %s" name dir))
      (json-encode (list (cons "path" (nyx-org-claude--s path)))))))

(defun nyx-org-claude-attach-delete (target name)
  "Delete attachment NAME from TARGET (destructive; removes the file).  Saves."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (unless (org-attach-dir)
      (error "nyx-org-claude: %s has no attachments" (org-get-heading t t t t)))
    (org-attach-delete-one name)
    (nyx-org-claude--save-current)
    (json-encode
     (cons (cons "deleted" (nyx-org-claude--s name))
           (nyx-org-claude--attach-alist)))))

;;;; ---- org-roam (notes) ------------------------------------------------

(defun nyx-org-claude--ensure-roam ()
  "Load org-roam (lazily) and confirm its API is available."
  (unless (featurep 'org-roam) (require 'org-roam nil t))
  (unless (fboundp 'org-roam-node-list)
    (error "nyx-org-claude: org-roam is not available")))

(defun nyx-org-claude--roam-node-alist (node)
  "Return org-roam NODE as a JSON-ready alist."
  (list (cons "id"      (nyx-org-claude--s (org-roam-node-id node)))
        (cons "title"   (nyx-org-claude--s (org-roam-node-title node)))
        (cons "file"    (nyx-org-claude--s (org-roam-node-file node)))
        (cons "level"   (or (org-roam-node-level node) 0))
        (cons "tags"    (apply #'vector (mapcar #'nyx-org-claude--s
                                                (org-roam-node-tags node))))
        (cons "aliases" (apply #'vector (mapcar #'nyx-org-claude--s
                                                (org-roam-node-aliases node))))))

(defun nyx-org-claude--roam-node (target)
  "Resolve TARGET to an `org-roam-node'.
TARGET is JSON {\"id\":...} or {\"title\":...}, or a bare title string."
  (nyx-org-claude--ensure-roam)
  (let* ((tgt (cond
               ((and (stringp target) (string-prefix-p "{" (string-trim target)))
                (json-parse-string target :object-type 'alist :array-type 'list))
               ((stringp target) `((title . ,target)))
               (t target)))
         (id (alist-get 'id tgt))
         (title (alist-get 'title tgt)))
    (cond
     ((and id (stringp id) (not (string-empty-p id)))
      (or (org-roam-node-from-id id)
          (error "nyx-org-claude: no roam node with id %s" id)))
     ((and title (stringp title) (not (string-empty-p title)))
      (or (org-roam-node-from-title-or-alias title)
          (error "nyx-org-claude: no roam node titled %s" title)))
     (t (error "nyx-org-claude: roam target needs \"id\" or \"title\"")))))

(defun nyx-org-claude-roam-list ()
  "Return JSON array of all org-roam nodes."
  (nyx-org-claude--ensure-roam)
  (json-encode (apply #'vector
                      (mapcar #'nyx-org-claude--roam-node-alist
                              (org-roam-node-list)))))

(defun nyx-org-claude-roam-find (str)
  "Return JSON array of nodes whose title/alias/tags contain STR (case-insensitive)."
  (nyx-org-claude--ensure-roam)
  (let ((needle (downcase (or str ""))))
    (json-encode
     (apply #'vector
            (delq nil
                  (mapcar
                   (lambda (n)
                     (let ((hay (downcase
                                 (string-join
                                  (append (list (or (org-roam-node-title n) ""))
                                          (org-roam-node-aliases n)
                                          (org-roam-node-tags n))
                                  " "))))
                       (when (string-search needle hay)
                         (nyx-org-claude--roam-node-alist n))))
                   (org-roam-node-list)))))))

(defun nyx-org-claude-roam-get (target)
  "Return the org-roam node at TARGET as JSON (incl. its file path to read)."
  (json-encode (nyx-org-claude--roam-node-alist (nyx-org-claude--roam-node target))))

(defun nyx-org-claude-roam-create (title &optional body)
  "Create a new file-level org-roam node titled TITLE with optional BODY.
Replicates the user's default capture template (ID drawer + #+title, filename
`<timestamp>-<slug>.org') and syncs the DB.  Returns the new node."
  (nyx-org-claude--ensure-roam)
  (let* ((dir   (expand-file-name org-roam-directory))
         (slug  (org-roam-node-slug (org-roam-node-create :title title)))
         (stamp (format-time-string "%Y%m%d%H%M%S"))
         (file  (expand-file-name (format "%s-%s.org" stamp slug) dir))
         (id    (org-id-new)))
    (when (file-exists-p file)
      (error "nyx-org-claude: %s already exists" file))
    (unless (file-directory-p dir) (make-directory dir t))
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID:       " id "\n:END:\n"
              "#+title: " title "\n")
      (when (and body (not (string-empty-p body)))
        (insert "\n" (string-trim-right body) "\n")))
    (org-roam-db-update-file file)
    (json-encode (nyx-org-claude--roam-node-alist (org-roam-node-from-id id)))))

(defun nyx-org-claude--roam-file-body-region ()
  "Return (BEG . END) of the file-level body in the current roam buffer.
BEG follows the top property drawer and leading #+keyword/blank lines;
END is the first heading or `point-max'."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^[ \t]*:PROPERTIES:")
      (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
      (forward-line 1))
    (while (and (not (eobp)) (looking-at-p "^[ \t]*\\(#\\+\\|$\\)"))
      (forward-line 1))
    (let ((beg (point))
          (end (if (re-search-forward "^\\*+ " nil t) (match-beginning 0)
                 (point-max))))
      (cons beg end))))

(defun nyx-org-claude-roam-update (target mode text)
  "Edit the body of the roam node at TARGET.  MODE is \"append\" or \"replace\".
For a heading-level node, edits the subtree body; for a file-level node, edits
the intro text before the first heading.  Saves and syncs the DB."
  (nyx-org-claude--ensure-roam)
  (let* ((node (nyx-org-claude--roam-node target))
         (file (org-roam-node-file node))
         (lvl  (or (org-roam-node-level node) 0)))
    (with-current-buffer (find-file-noselect file)
      (if (> lvl 0)
          (org-with-point-at (org-roam-node-point node)
            (org-back-to-heading t)
            (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                  (end (save-excursion (outline-next-heading) (point))))
              (if (string= mode "replace")
                  (progn (delete-region beg end) (goto-char beg)
                         (insert (string-trim-right text) "\n"))
                (goto-char end) (unless (bolp) (insert "\n"))
                (insert (string-trim-right text) "\n"))))
        (let* ((r (nyx-org-claude--roam-file-body-region))
               (beg (car r)) (end (cdr r)))
          (if (string= mode "replace")
              (progn (delete-region beg end) (goto-char beg)
                     (insert (string-trim-right text) "\n\n"))
            (goto-char end) (skip-chars-backward " \t\n")
            (insert "\n" (string-trim-right text) "\n"))))
      (save-buffer)
      (org-roam-db-update-file file))
    (json-encode (nyx-org-claude--roam-node-alist (nyx-org-claude--roam-node target)))))

(defun nyx-org-claude-roam-backlinks (target)
  "Return JSON array of backlinks pointing TO the node at TARGET."
  (nyx-org-claude--ensure-roam)
  (let ((links (org-roam-backlinks-get (nyx-org-claude--roam-node target) :unique t)))
    (json-encode
     (apply #'vector
            (mapcar
             (lambda (bl)
               (let* ((src (org-roam-backlink-source-node bl))
                      (props (org-roam-backlink-properties bl))
                      (outline (and (listp props) (plist-get props :outline))))
                 (list (cons "source_id"    (nyx-org-claude--s (org-roam-node-id src)))
                       (cons "source_title" (nyx-org-claude--s (org-roam-node-title src)))
                       (cons "source_file"  (nyx-org-claude--s (org-roam-node-file src)))
                       (cons "point"        (or (org-roam-backlink-point bl) 0))
                       (cons "outline"      (apply #'vector
                                                   (mapcar #'nyx-org-claude--s outline))))))
             links)))))

;;;; ---- linking ---------------------------------------------------------

(defun nyx-org-claude-link-add (target kind dest &optional desc)
  "Append an Org link to TARGET's body and return TARGET's entry.
KIND selects how DEST is interpreted (and the default description):
- \"url\"    DEST is a full URL (PR, commit, web mail) -> [[URL][DESC]]
- \"mailto\" DEST is an email address                  -> [[mailto:DEST][DESC|DEST]]
- \"file\"   DEST is a path, optionally PATH::SEARCH    -> [[file:DEST][DESC|basename]]
- \"id\"     DEST is a raw Org/roam ID                  -> [[id:DEST][DESC|DEST]]
- \"entry\"  DEST is target JSON {id|file,path}; ensures it has an ID, links by
             id, DESC defaults to its heading
- \"roam\"   DEST is a roam node ({id|title} or bare title); links by id, DESC
             defaults to the node title.
Single-entry edit: auto-saves (the destination's new ID, for \"entry\", too)."
  (when (and (stringp desc) (string-empty-p desc)) (setq desc nil))
  (let (link txt)
    (pcase kind
      ("url"    (setq link (cons dest (or desc dest))))
      ("mailto" (setq link (cons (concat "mailto:" dest) (or desc dest))))
      ("file"   (setq link (cons (concat "file:" dest)
                                 (or desc (file-name-nondirectory
                                           (car (split-string dest "::")))))))
      ("id"     (setq link (cons (concat "id:" dest) (or desc dest))))
      ("entry"  (org-with-point-at (nyx-org-claude--target dest)
                  (org-back-to-heading t)
                  (let ((id (org-id-get-create))
                        (h  (org-get-heading t t t t)))
                    (nyx-org-claude--save-current)
                    (setq link (cons (concat "id:" id) (or desc h))))))
      ("roam"   (let ((n (nyx-org-claude--roam-node dest)))
                  (setq link (cons (concat "id:" (org-roam-node-id n))
                                   (or desc (org-roam-node-title n))))))
      (_ (error "nyx-org-claude: link kind must be url/mailto/file/id/entry/roam")))
    (setq txt (format "[[%s][%s]]" (car link) (cdr link)))
    (nyx-org-claude--at-target target
      (let ((end (save-excursion (outline-next-heading) (point))))
        (goto-char end) (unless (bolp) (insert "\n"))
        (insert txt "\n")))))

(defun nyx-org-claude-links (target)
  "Return JSON array of Org links in TARGET's subtree: {type, raw, description}."
  (org-with-point-at (nyx-org-claude--target target)
    (org-back-to-heading t)
    (org-narrow-to-subtree)
    (unwind-protect
        (let (out)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (l)
              (let ((cb (org-element-property :contents-begin l))
                    (ce (org-element-property :contents-end l)))
                (push (list (cons "type" (nyx-org-claude--s (org-element-property :type l)))
                            (cons "raw"  (nyx-org-claude--s (org-element-property :raw-link l)))
                            (cons "description"
                                  (if (and cb ce)
                                      (nyx-org-claude--s
                                       (string-trim (buffer-substring-no-properties cb ce)))
                                    "")))
                      out))))
          (json-encode (apply #'vector (nreverse out))))
      (widen))))

;;;; ---- save / status ---------------------------------------------------

(defun nyx-org-claude-save (&optional file)
  "Save FILE's buffer (relative to `org-directory'), or all Org buffers."
  (if (and file (not (string-empty-p file)))
      (let* ((abs (if (file-name-absolute-p file) file
                    (expand-file-name file org-directory)))
             (buf (find-buffer-visiting abs)))
        (when buf (with-current-buffer buf (save-buffer)))
        (json-encode `(("saved" . ,(if buf file "no-such-buffer")))))
    (org-save-all-org-buffers)
    (json-encode '(("saved" . "all-org-buffers")))))

(defun nyx-org-claude-dirty ()
  "Return JSON array of Org files with unsaved changes."
  (json-encode
   (apply #'vector
          (delq nil
                (mapcar (lambda (b)
                          (with-current-buffer b
                            (when (and (derived-mode-p 'org-mode)
                                       (buffer-modified-p)
                                       (buffer-file-name))
                              (nyx-org-claude--s (buffer-file-name)))))
                        (buffer-list))))))

(defun nyx-org-claude--to-file (file str)
  "Write STR to FILE (used by the shell wrapper to avoid escaping)."
  (let ((coding-system-for-write 'utf-8))
    (write-region (or str "") nil file nil 'quiet))
  t)

(provide 'nyx-org-claude)
;;; nyx-org-claude.el ends here
