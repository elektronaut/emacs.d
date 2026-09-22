;;; nyx-review.el --- Review comments on code -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Annotates code with review comments and collects them into a single
;;;   prompt.  Comments are written either on a Magit diff hunk or on the lines
;;;   of a file being read, and both kinds land in the same review, keyed by
;;;   repository.
;;;
;;;   A comment records the line of the worktree file it refers to, so the
;;;   prompt points where an agent will look.  How it finds its way back on
;;;   screen differs: a diff comment re-anchors to the hunk it was written on,
;;;   a buffer comment follows a marker while the buffer lives and its own text
;;;   once the buffer has been closed and reopened.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'transient)

(eval-when-compile (require 'magit nil t))

(declare-function magit-current-section "magit-section")
(declare-function magit-diff-hunk-line "magit-diff" (section goto-from))
(declare-function magit-diff-hunk-region-beginning "magit-diff")
(declare-function magit-diff-hunk-region-end "magit-diff")
(declare-function magit-diff-scope "magit-diff" (&optional section strict))
(declare-function magit-diff-visit--offset "magit-diff" (line file &rest args))
(declare-function magit-diff-visit--sides "magit-diff")
(declare-function magit-hunk-section-p "magit-diff" (obj))
(declare-function magit-toplevel "magit-git" (&optional directory))

(defvar magit-mode-map)
(defvar magit-root-section)


;;
;; Options
;;

(defgroup nyx-review nil
  "Review comments on code."
  :group 'tools)

(defcustom nyx-review-preamble
  "Code review of the current changes. Address every comment below."
  "Text placed above the comments in the generated prompt."
  :type 'string
  :group 'nyx-review)

(defcustom nyx-review-send-function #'nyx-review-copy
  "Function `nyx-review-send' hands the generated prompt to."
  :type 'function
  :group 'nyx-review)

(defface nyx-review-comment
  '((t :inherit (font-lock-comment-face hl-line) :slant italic :extend t))
  "Face for a review comment shown below the lines it refers to."
  :group 'nyx-review)

(defface nyx-review-annotated
  '((t :inherit magit-diff-hunk-region))
  "Face for the lines a review comment refers to."
  :group 'nyx-review)


;;
;; Comments
;;

(cl-defstruct (nyx-review-note
               (:constructor nyx-review-note--make)
               (:copier nil))
  "A review comment on a range of lines.
A comment written on a diff carries the HUNK it was written on and its
OFFSET within it; one written in a file buffer carries a MARKER instead,
which tracks the lines as they are edited."
  file line end-line code text lines hunk offset marker)

(defvar nyx-review--notes (make-hash-table :test 'equal)
  "Maps a repository root to its list of `nyx-review-note' structs.")

(defun nyx-review--root ()
  "Return the repository the current buffer belongs to."
  (require 'magit)
  (or (magit-toplevel) (user-error "Not inside a Git repository")))

(defun nyx-review--note< (a b)
  "Return non-nil if note A comes before note B in a review."
  (let ((file-a (nyx-review-note-file a))
        (file-b (nyx-review-note-file b)))
    (if (equal file-a file-b)
        (< (or (car (nyx-review--lines a)) 0) (or (car (nyx-review--lines b)) 0))
      (string< file-a file-b))))

(defun nyx-review-notes (&optional root)
  "Return the notes recorded for ROOT, in reading order."
  (sort (copy-sequence (gethash (or root (nyx-review--root)) nyx-review--notes))
        #'nyx-review--note<))

(defun nyx-review--notes-for-file (file)
  "Return every note recorded against FILE, in any repository."
  (let ((found '()))
    (maphash (lambda (_root notes)
               (dolist (note notes)
                 (when (equal (nyx-review-note-file note) file)
                   (push note found))))
             nyx-review--notes)
    found))

(defun nyx-review--add (note)
  "Record NOTE for the current repository."
  (let ((root (nyx-review--root)))
    (puthash root (cons note (gethash root nyx-review--notes)) nyx-review--notes)))

(defun nyx-review--remove (note)
  "Discard NOTE from the current repository."
  (let ((root (nyx-review--root))
        (marker (nyx-review-note-marker note)))
    (when marker (set-marker marker nil))
    (puthash root (delq note (gethash root nyx-review--notes)) nyx-review--notes)))

(defun nyx-review--lines (note)
  "Return NOTE's first and last line as a cons.
A note that tracks a live buffer is asked where its marker ended up, so
comments stay accurate while the file is edited underneath them."
  (let ((marker (nyx-review-note-marker note)))
    (if (and marker (marker-buffer marker) (buffer-live-p (marker-buffer marker)))
        (with-current-buffer (marker-buffer marker)
          (let ((line (line-number-at-pos (marker-position marker) t)))
            (cons line (+ line (1- (nyx-review-note-lines note))))))
      (cons (nyx-review-note-line note) (nyx-review-note-end-line note)))))

(defun nyx-review--location (note &optional root)
  "Return NOTE's location as a FILE:LINE reference relative to ROOT."
  (pcase-let ((`(,line . ,end) (nyx-review--lines note))
              (file (file-relative-name (nyx-review-note-file note)
                                        (or root (nyx-review--root)))))
    (cond ((null line) file)
          ((and end (> end line)) (format "%s:%d-%d" file line end))
          (t (format "%s:%d" file line)))))


;;
;; Diffs
;;

(defun nyx-review--hunk-at (pos)
  "Return the hunk section at POS, if there is one.
`magit-diff--hunk-section' is not used because it returns nil whenever
the region selects part of a hunk, the very case this has to handle."
  (save-excursion
    (goto-char pos)
    (let ((section (magit-current-section)))
      (and (magit-hunk-section-p section) section))))

(defun nyx-review--hunk-file (hunk)
  "Return the absolute path of the file HUNK belongs to."
  (expand-file-name (oref (oref hunk parent) value) (magit-toplevel)))

(defun nyx-review--hunk-header (hunk)
  "Return the heading line of HUNK."
  (save-excursion
    (goto-char (oref hunk start))
    (buffer-substring-no-properties (pos-bol) (pos-eol))))

(defun nyx-review--worktree-line (hunk pos)
  "Return the worktree line number of the diff line at POS within HUNK.
The region is suppressed while Magit resolves the two sides of the diff,
because it reports no diff scope at all while part of a hunk is selected."
  (save-excursion
    (goto-char pos)
    (goto-char (pos-bol))
    (let ((transient-mark-mode nil))
      (pcase-let* ((`(,_ ,new) (magit-diff-visit--sides))
                   (`(,rev ,file) new)
                   (line (magit-diff-hunk-line hunk nil)))
        (and line
             (pcase rev
               ("{worktree}" line)
               ("{index}" (magit-diff-visit--offset line file))
               (_ (magit-diff-visit--offset line file rev))))))))

(defun nyx-review--diff-bounds ()
  "Return the positions of the diff lines to comment on at point.
The region is honoured when it selects lines inside a single hunk."
  (if (eq (magit-diff-scope) 'region)
      (cons (magit-diff-hunk-region-beginning) (magit-diff-hunk-region-end))
    (cons (pos-bol) (pos-eol))))

(defun nyx-review--capture-diff ()
  "Return a note for the diff lines at point, its comment still unwritten.
The hunk is looked up at the first of those lines, because the end of a
region can fall on the boundary between two sections."
  (let* ((bounds (nyx-review--diff-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (hunk (or (nyx-review--hunk-at beg)
                   (user-error "No diff hunk at point"))))
    (when (< beg (oref hunk content))
      (user-error "Point is on the hunk heading, not on a line of the diff"))
    (nyx-review-note--make
     :file (nyx-review--hunk-file hunk)
     :line (nyx-review--worktree-line hunk beg)
     :end-line (nyx-review--worktree-line hunk end)
     :code (buffer-substring-no-properties beg end)
     :hunk (nyx-review--hunk-header hunk)
     :offset (count-lines (oref hunk start) beg)
     :lines (max 1 (count-lines beg end)))))


;;
;; Buffers
;;

(defun nyx-review--buffer-bounds ()
  "Return the positions of the lines to comment on in this buffer.
A region ending at the beginning of a line stops on the line before it,
which is where it looks like it ends."
  (if (use-region-p)
      (cons (save-excursion (goto-char (region-beginning)) (pos-bol))
            (save-excursion
              (goto-char (region-end))
              (when (and (bolp) (> (point) (region-beginning)))
                (forward-line -1))
              (pos-eol)))
    (cons (pos-bol) (pos-eol))))

(defun nyx-review--capture-buffer ()
  "Return a note for the lines at point, its comment still unwritten."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (let* ((bounds (nyx-review--buffer-bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (nyx-review-note--make
     :file (expand-file-name buffer-file-name)
     :line (line-number-at-pos beg t)
     :end-line (line-number-at-pos end t)
     :code (buffer-substring-no-properties beg end)
     :marker (copy-marker beg)
     :lines (max 1 (count-lines beg end)))))

(defun nyx-review--anchor (note)
  "Return the position NOTE refers to in the current buffer.
A note whose marker has been lost is located by its own first line,
falling back to the line it was written on."
  (let ((marker (nyx-review-note-marker note)))
    (if (eq (marker-buffer marker) (current-buffer))
        (marker-position marker)
      (let ((wanted (car (split-string (nyx-review-note-code note) "\n")))
            (position nil))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- (nyx-review-note-line note)))
            (setq position
                  (if (equal (buffer-substring-no-properties (pos-bol) (pos-eol))
                             wanted)
                      (pos-bol)
                    (goto-char (point-min))
                    (and (not (string-blank-p wanted))
                         (search-forward wanted nil t)
                         (pos-bol))))))
        (when position
          (set-marker (nyx-review-note-marker note) position))
        position))))

(defun nyx-review--capture ()
  "Return a note for whatever is at point, its comment still unwritten."
  (if (derived-mode-p 'magit-mode)
      (nyx-review--capture-diff)
    (nyx-review--capture-buffer)))


;;
;; Overlays
;;

(defun nyx-review-note-at-point ()
  "Return the note the line at point carries, if any."
  (seq-some (lambda (overlay) (overlay-get overlay 'nyx-review))
            (overlays-in (pos-bol) (min (point-max) (1+ (pos-eol))))))

(defun nyx-review--comment-string (note)
  "Return the string NOTE's comment is displayed as.
Every line is terminated by a newline of its own, so the background of
`nyx-review-comment' reaches the edge of the window."
  (propertize
   (mapconcat (lambda (line) (concat "  ▏ " line "\n"))
              (split-string (nyx-review-note-text note) "\n")
              "")
   'face 'nyx-review-comment))

(defun nyx-review--draw (note beg end)
  "Draw NOTE over the lines from BEG to END.
The overlay reaches past the newline ending END so that the comment is
displayed on the lines below it rather than at the end of the last one."
  (let ((overlay (make-overlay beg (min (point-max) (1+ end)))))
    (overlay-put overlay 'nyx-review note)
    (overlay-put overlay 'nyx-review-overlay t)
    (overlay-put overlay 'face 'nyx-review-annotated)
    (overlay-put overlay 'after-string (nyx-review--comment-string note))))

(defun nyx-review--draw-from (note position)
  "Draw NOTE over the lines it covers, starting at POSITION."
  (save-excursion
    (goto-char position)
    (nyx-review--draw note
                      (pos-bol)
                      (save-excursion
                        (forward-line (1- (nyx-review-note-lines note)))
                        (pos-eol)))))

(defun nyx-review--draw-in-hunk (note hunk)
  "Draw NOTE over the lines it refers to within HUNK."
  (save-excursion
    (goto-char (oref hunk start))
    (forward-line (nyx-review-note-offset note))
    (let ((beg (pos-bol))
          (end (save-excursion
                 (forward-line (1- (nyx-review-note-lines note)))
                 (pos-eol))))
      (when (and (>= beg (oref hunk content)) (<= end (oref hunk end)))
        (nyx-review--draw note beg end)))))

(defun nyx-review--hunks ()
  "Return the hunk sections of the current buffer."
  (let ((hunks '()))
    (letrec ((walk (lambda (section)
                     (if (magit-hunk-section-p section)
                         (push section hunks)
                       (mapc walk (oref section children))))))
      (when (bound-and-true-p magit-root-section)
        (funcall walk magit-root-section)))
    (nreverse hunks)))

(defun nyx-review--clear-overlays ()
  "Remove the review overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'nyx-review-overlay t))

(defun nyx-review--render-diff ()
  "Draw the review comments that belong to this Magit buffer."
  (when (derived-mode-p 'magit-mode)
    (nyx-review--clear-overlays)
    (when-let* (((not (hash-table-empty-p nyx-review--notes)))
                (root (magit-toplevel))
                (notes (gethash root nyx-review--notes)))
      (dolist (hunk (nyx-review--hunks))
        (let ((file (nyx-review--hunk-file hunk))
              (header (nyx-review--hunk-header hunk)))
          (dolist (note notes)
            (when (and (equal file (nyx-review-note-file note))
                       (equal header (nyx-review-note-hunk note)))
              (nyx-review--draw-in-hunk note hunk))))))))

(defun nyx-review--render-buffer ()
  "Draw the review comments that belong to this file buffer."
  (when buffer-file-name
    (nyx-review--clear-overlays)
    (unless (hash-table-empty-p nyx-review--notes)
      (dolist (note (nyx-review--notes-for-file (expand-file-name buffer-file-name)))
        (when-let* ((position (nyx-review--anchor note)))
          (nyx-review--draw-from note position))))))

(defun nyx-review-redraw ()
  "Redraw the review comments in every buffer that has any."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((derived-mode-p 'magit-mode) (nyx-review--render-diff))
            (buffer-file-name (nyx-review--render-buffer))))))


;;
;; Editing
;;

(defvar-local nyx-review--callback nil
  "Function the comment being edited is handed to.")

(defvar-local nyx-review--origin nil
  "Buffer the comment being edited was started from.")

(defvar-keymap nyx-review-edit-mode-map
  :doc "Keymap for `nyx-review-edit-mode'."
  "C-c C-c" #'nyx-review-edit-finish
  "C-c C-k" #'nyx-review-edit-abort)

(define-derived-mode nyx-review-edit-mode text-mode "Review"
  "Major mode for writing a review comment."
  (setq-local fill-column 72))

(defun nyx-review--edit (label initial callback)
  "Read a comment on LABEL, starting from INITIAL, and pass it to CALLBACK."
  (let ((buffer (get-buffer-create "*review comment*"))
        (origin (current-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (nyx-review-edit-mode)
      (when initial (insert initial))
      (setq nyx-review--callback callback
            nyx-review--origin origin)
      (setq-local header-line-format
                  (substitute-command-keys
                   (format "%s — finish with \\[nyx-review-edit-finish], \
abort with \\[nyx-review-edit-abort]" label))))
    (select-window
     (display-buffer buffer '(display-buffer-below-selected
                              (window-height . 10))))))

(defun nyx-review-edit-finish ()
  "Record the comment being edited."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (callback nyx-review--callback)
        (origin nyx-review--origin))
    (when (string-empty-p text)
      (user-error "The comment is empty"))
    (quit-window t)
    (when (buffer-live-p origin)
      (with-current-buffer origin
        (funcall callback text)))))

(defun nyx-review-edit-abort ()
  "Discard the comment being edited."
  (interactive)
  (quit-window t))


;;
;; Prompt
;;

(defun nyx-review--format (note root)
  "Return NOTE formatted for a prompt, with its path relative to ROOT."
  (concat (nyx-review--location note root) "\n"
          (mapconcat (lambda (line) (concat "> " line))
                     (split-string (nyx-review-note-code note) "\n")
                     "\n")
          "\n\n"
          (nyx-review-note-text note)
          "\n"))

(defun nyx-review-prompt (&optional root)
  "Return the review comments recorded for ROOT as a single prompt."
  (let* ((root (or root (nyx-review--root)))
         (notes (nyx-review-notes root)))
    (unless notes
      (user-error "No review comments"))
    (concat nyx-review-preamble "\n\n"
            (mapconcat (lambda (note) (nyx-review--format note root)) notes "\n"))))

(defun nyx-review-copy (prompt)
  "Copy PROMPT to the kill ring and display it."
  (kill-new prompt)
  (with-current-buffer (get-buffer-create "*review prompt*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min)))
    (display-buffer (current-buffer)))
  (message "Review prompt copied to the kill ring"))


;;
;; Commands
;;

(defun nyx-review-comment ()
  "Add or edit a review comment on the lines at point."
  (interactive)
  (if-let* ((note (nyx-review-note-at-point)))
      (nyx-review--edit (nyx-review--location note)
                        (nyx-review-note-text note)
                        (lambda (text)
                          (setf (nyx-review-note-text note) text)
                          (nyx-review-redraw)))
    (let ((note (nyx-review--capture)))
      (nyx-review--edit (nyx-review--location note)
                        nil
                        (lambda (text)
                          (setf (nyx-review-note-text note) text)
                          (nyx-review--add note)
                          (nyx-review-redraw))))))

(defun nyx-review-remove ()
  "Remove the review comment at point."
  (interactive)
  (nyx-review--remove (or (nyx-review-note-at-point)
                          (user-error "No review comment at point")))
  (nyx-review-redraw))

(defun nyx-review-clear (&optional root)
  "Remove every review comment recorded for ROOT."
  (interactive)
  (dolist (note (gethash (or root (nyx-review--root)) nyx-review--notes))
    (when-let* ((marker (nyx-review-note-marker note)))
      (set-marker marker nil)))
  (remhash (or root (nyx-review--root)) nyx-review--notes)
  (nyx-review-redraw))

(defun nyx-review-send ()
  "Hand the review comments to `nyx-review-send-function'."
  (interactive)
  (let ((root (nyx-review--root)))
    (funcall nyx-review-send-function (nyx-review-prompt root))
    (when (y-or-n-p "Clear the review comments? ")
      (nyx-review-clear root))))

(transient-define-prefix nyx-review-transient ()
  "Review comments"
  [["Comment"
    ("," "Add or edit" nyx-review-comment)
    ("k" "Remove" nyx-review-remove)
    ("K" "Remove all" nyx-review-clear)]
   ["Review"
    ("s" "Send" nyx-review-send)
    ("g" "Redraw" nyx-review-redraw)]])

(keymap-global-set "C-c v" #'nyx-review-comment)
(keymap-global-set "C-c V" #'nyx-review-transient)

(add-hook 'find-file-hook #'nyx-review--render-buffer)

(with-eval-after-load 'magit
  (keymap-set magit-mode-map "," #'nyx-review-comment)
  (keymap-set magit-mode-map ";" #'nyx-review-transient)
  (add-hook 'magit-refresh-buffer-hook #'nyx-review--render-diff))

(provide 'nyx-review)
;;; nyx-review.el ends here
