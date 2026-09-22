;;; nyx-worktree.el --- Git worktree awareness -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Identifies the repository and worktree a directory belongs to, by asking
;;;   git rather than inspecting the path, and derives display names from that.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct (project-worktree
               (:constructor project-worktree--make)
               (:copier nil))
  "A single git worktree."
  root repo-root repo-name name short-name branch main)

(defvar project-worktree--cache (make-hash-table :test 'equal)
  "Maps a repository root to its list of `project-worktree' structs.")

(defvar project-worktree--repo-cache (make-hash-table :test 'equal)
  "Maps a directory to its repository root, or the symbol `none'.")


;;
;; Git
;;

(defun project-worktree--git (dir &rest args)
  "Run git in DIR with ARGS, returning trimmed output, or nil on failure."
  (when (and dir (not (file-remote-p dir)) (file-directory-p dir))
    (let ((default-directory dir))
      (with-temp-buffer
        (when (eq 0 (apply #'process-file "git" nil t nil args))
          (string-trim (buffer-string)))))))

(defun project-worktree--repo-root (dir)
  "Return the main worktree root of the repository containing DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (cached (gethash dir project-worktree--repo-cache 'miss)))
    (if (not (eq cached 'miss))
        (and (stringp cached) cached)
      (let* ((common (project-worktree--git
                      dir "rev-parse" "--path-format=absolute" "--git-common-dir"))
             (root (and common
                        (directory-file-name
                         (file-name-directory (directory-file-name common))))))
        (puthash dir (or root 'none) project-worktree--repo-cache)
        root))))


;;
;; Names
;;

(defun project-worktree--names (roots)
  "Return a name per root in ROOTS.
The name is the innermost path component that is not shared by every
worktree, so layouts that repeat the repository name in each checkout,
like <workspace>/<repo>, still tell their worktrees apart."
  (let ((tails (mapcar (lambda (root) (reverse (split-string root "/" t))) roots))
        (depth 0))
    (while (and (cdr tails)
                (cl-every (lambda (tail) (nthcdr (1+ depth) tail)) tails)
                (let ((component (nth depth (car tails))))
                  (cl-every (lambda (tail) (equal (nth depth tail) component)) tails)))
      (setq depth (1+ depth)))
    (mapcar (lambda (tail) (nth depth tail)) tails)))

(defun project-worktree--strip-repo (name repo-name)
  "Remove REPO-NAME from NAME as a whole component."
  (if (equal name repo-name)
      name
    (let ((parts (seq-remove (lambda (part) (equal part repo-name))
                             (split-string name "-"))))
      (if parts (string-join parts "-") name))))

(defun project-worktree--short-names (names repo-name)
  "Return NAMES with REPO-NAME stripped, or NAMES if that would collide."
  (let ((stripped (mapcar (lambda (name) (project-worktree--strip-repo name repo-name))
                          names)))
    (if (eq (length (seq-uniq stripped)) (length stripped)) stripped names)))

(defconst project-worktree--ticket-number "\\`[0-9]+\\'")
(defconst project-worktree--ticket-key "\\`[A-Z][A-Z0-9]*\\'")

(defun project-worktree--pinned-head (parts)
  "Return how many leading PARTS form a ticket reference."
  (cond ((and parts (string-match-p project-worktree--ticket-number (car parts))) 1)
        ((and (cdr parts)
              (string-match-p project-worktree--ticket-key (car parts))
              (string-match-p project-worktree--ticket-number (cadr parts)))
         2)
        (t 0)))

(defun project-worktree--clip (string budget)
  "Cut STRING to BUDGET columns."
  (cond ((<= (length string) budget) string)
        ((< budget 2) "")
        (t (concat (substring string 0 (1- budget)) "…"))))

(defun project-worktree-truncate (name budget)
  "Shorten NAME to at most BUDGET columns.
Drops whole components from the middle, keeping any leading ticket
reference and the final component intact for as long as they fit."
  (if (<= (length name) budget)
      name
    (let* ((parts (split-string name "-"))
           (last (car (last parts)))
           (pinned (project-worktree--pinned-head parts)))
      (if (< (length parts) 2)
          (project-worktree--clip name budget)
        (or (cl-loop for n from (1- (length parts)) downto (max pinned 1)
                     for candidate = (concat (string-join (seq-take parts n) "-") "…" last)
                     when (<= (length candidate) budget) return candidate)
            (when (> pinned 0)
              (project-worktree--clip
               (concat (string-join (seq-take parts pinned) "-") "…") budget))
            (let ((head (- budget (length last) 1)))
              (when (>= head 2)
                (concat (substring (car parts) 0 head) "…" last)))
            (project-worktree--clip name budget))))))


;;
;; Worktrees
;;

(defun project-worktree--parse (repo-root output)
  "Parse porcelain OUTPUT into `project-worktree' structs for REPO-ROOT."
  (let ((repo-name (file-name-nondirectory repo-root))
        (entries '())
        root branch)
    (dolist (line (split-string output "\n"))
      (cond
       ((string-prefix-p "worktree " line)
        (setq root (directory-file-name (substring line 9))))
       ((string-prefix-p "branch " line)
        (setq branch (replace-regexp-in-string "\\`refs/heads/" "" (substring line 7))))
       ((and (string-empty-p line) root)
        (push (list root branch) entries)
        (setq root nil branch nil))))
    (when root (push (list root branch) entries))
    (setq entries (nreverse entries))
    (let* ((names (project-worktree--names (mapcar #'car entries)))
           (short-names (project-worktree--short-names names repo-name)))
      (seq-map-indexed
       (lambda (entry index)
         (project-worktree--make
          :root (car entry)
          :repo-root repo-root
          :repo-name repo-name
          :name (nth index names)
          :short-name (nth index short-names)
          :branch (cadr entry)
          :main (eq index 0)))
       entries))))

(defun project-worktree-list (repo-root)
  "Return the worktrees of the repository at REPO-ROOT."
  (let ((cached (gethash repo-root project-worktree--cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let* ((output (project-worktree--git repo-root "worktree" "list" "--porcelain"))
             (worktrees (and output (project-worktree--parse repo-root output))))
        (puthash repo-root worktrees project-worktree--cache)
        worktrees))))

(defun project-worktree--enclosing (dir worktrees)
  "Return the innermost of WORKTREES containing DIR."
  (car (sort (seq-filter
              (lambda (worktree)
                (string-prefix-p (file-name-as-directory (project-worktree-root worktree))
                                 (file-name-as-directory dir)))
              worktrees)
             (lambda (a b) (> (length (project-worktree-root a))
                              (length (project-worktree-root b)))))))

(defun project-worktree--known (dir)
  "Return an already cached worktree containing DIR, without consulting git."
  (let ((candidates '()))
    (maphash (lambda (_root worktrees) (setq candidates (append worktrees candidates)))
             project-worktree--cache)
    (project-worktree--enclosing dir candidates)))

(defun project-worktree--cached-repo-root (dir)
  "Return the repository root cached for DIR, without consulting git."
  (let ((cached (gethash (file-name-as-directory dir) project-worktree--repo-cache)))
    (and (stringp cached) cached)))

(defun project-worktree-at (dir)
  "Return the `project-worktree' containing DIR, if any.
Resolution goes cheapest first: the repository already recorded for DIR,
then the worktrees already known, and only then git."
  (when-let* ((dir (expand-file-name (or dir default-directory))))
    (or (when-let* ((repo-root (project-worktree--cached-repo-root dir)))
          (project-worktree--enclosing dir (project-worktree-list repo-root)))
        (when-let* ((worktree (project-worktree--known dir)))
          (puthash (file-name-as-directory dir)
                   (project-worktree-repo-root worktree)
                   project-worktree--repo-cache)
          worktree)
        (when-let* ((repo-root (project-worktree--repo-root dir))
                    (worktrees (project-worktree-list repo-root)))
          (project-worktree--enclosing dir worktrees)))))

(defun project-worktree-current ()
  "Return the `project-worktree' for the current buffer, if any."
  (project-worktree-at default-directory))

(defun project-worktree-siblings (worktree)
  "Return the other worktrees sharing a repository with WORKTREE."
  (seq-remove (lambda (other)
                (equal (project-worktree-root other) (project-worktree-root worktree)))
              (project-worktree-list (project-worktree-repo-root worktree))))

(defun project-worktree-linked-p (worktree)
  "Return non-nil if WORKTREE is not its repository's main worktree."
  (not (project-worktree-main worktree)))

(defun project-worktree-live-p (worktree)
  "Return non-nil if WORKTREE still exists on disk."
  (file-directory-p (project-worktree-root worktree)))

(defun project-worktree-persp-name (worktree)
  "Return the perspective name for WORKTREE.
Stable regardless of how many siblings the repository has."
  (let ((repo (project-worktree-repo-name worktree)))
    (if (project-worktree-main worktree)
        repo
      (concat repo "·" (project-worktree-short-name worktree)))))

(defun project-worktree-display-name (worktree &optional budget)
  "Return the display name for WORKTREE, fitting the name within BUDGET."
  (let ((repo (project-worktree-repo-name worktree)))
    (if (and (project-worktree-main worktree)
             (null (project-worktree-siblings worktree)))
        repo
      (let ((name (if (project-worktree-main worktree)
                      "main"
                    (project-worktree-short-name worktree))))
        (concat repo "·" (if budget (project-worktree-truncate name budget) name))))))

(defun project-worktree-forget (root)
  "Discard cached worktree information for the repository at ROOT."
  (remhash root project-worktree--cache)
  (let (stale)
    (maphash (lambda (dir cached) (when (equal cached root) (push dir stale)))
             project-worktree--repo-cache)
    (dolist (dir stale) (remhash dir project-worktree--repo-cache))))

(defun project-worktree--normalize (name)
  "Strip the decoration git and tooling add around NAME."
  (replace-regexp-in-string "/" "-" (replace-regexp-in-string "\\`worktree-" "" name)))

(defun project-worktree-branch-informative-p (worktree)
  "Return non-nil if WORKTREE's branch says more than its name already does."
  (when-let* ((branch (project-worktree-branch worktree)))
    (not (if (project-worktree-main worktree)
             (member branch '("main" "master"))
           (let ((branch (project-worktree--normalize branch)))
             (or (equal branch (project-worktree--normalize
                                (project-worktree-short-name worktree)))
                 (equal branch (project-worktree--normalize
                                (project-worktree-name worktree)))))))))

(defun project-worktree-label (worktree)
  "Return the completion label for WORKTREE.
The branch is appended only when it is not implied by the name."
  (let ((base (project-worktree-persp-name worktree)))
    (cond ((null (project-worktree-branch worktree)) (concat base " (detached)"))
          ((project-worktree-branch-informative-p worktree)
           (format "%s (%s)" base (project-worktree-branch worktree)))
          (t base))))

(defun project-worktree-refresh ()
  "Discard all cached worktree information."
  (interactive)
  (clrhash project-worktree--cache)
  (clrhash project-worktree--repo-cache))

(defun project-worktree--refresh-current (&rest _)
  "Discard cached worktree information for the repository we are in."
  (when-let* ((root (project-worktree--repo-root default-directory)))
    (project-worktree-forget root)))

(defun project-worktree-warm (roots)
  "Populate the worktree cache for the repositories containing ROOTS."
  (dolist (root roots)
    (when (file-directory-p root)
      (when-let* ((repo-root (project-worktree--repo-root root)))
        (project-worktree-list repo-root)))))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'project-worktree--refresh-current))

(provide 'nyx-worktree)
;;; nyx-worktree.el ends here
