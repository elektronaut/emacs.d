;;; nyx-project.el --- Project -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-consult)
(require 'nyx-worktree)

(defun project-display-name (dir)
  "Perspective name for the project in DIR."
  (if-let* ((worktree (project-worktree-at dir)))
      (project-worktree-persp-name worktree)
    (file-name-nondirectory (directory-file-name dir))))

(defun project--worktree-name (orig project)
  "Name PROJECT after its worktree, unless an explicit name is configured."
  (let ((name (funcall orig project)))
    (if-let* ((root (ignore-errors (project-root project)))
              ((equal name (file-name-nondirectory (directory-file-name root))))
              (worktree (project-worktree-at root)))
        (project-worktree-persp-name worktree)
      name)))

(advice-add #'project-name :around #'project--worktree-name)

(defun project-workspace--parent (root)
  "Return the directory holding ROOT."
  (file-name-directory (directory-file-name (expand-file-name root))))

(defun project-workspace-of (root)
  "Return the workspace ROOT belongs to, or nil if it stands alone.
A workspace is a directory holding several checkouts side by side, as
<workspace>/<repo> does for a set of worktrees. A directory becomes one
only once it holds more than one known project, so a lone checkout never
makes a workspace of wherever it happens to live."
  (let ((parent (project-workspace--parent root)))
    (when (> (seq-count (lambda (other)
                          (equal (project-workspace--parent other) parent))
                        (project-known-project-roots))
             1)
      parent)))

(defun project-workspace-name (workspace)
  "Return the name of WORKSPACE."
  (file-name-nondirectory (directory-file-name workspace)))

(defun project-workspace-contains-p (workspace root)
  "Return non-nil if ROOT is a project of WORKSPACE."
  (equal (project-workspace--parent root)
         (file-name-as-directory (expand-file-name workspace))))

(defun project-worktree-warm-known ()
  "Populate the worktree cache for all known projects."
  (project-worktree-warm (project-known-project-roots)))

(run-with-idle-timer 5 nil #'project-worktree-warm-known)

;; From https://andreyor.st/posts/2022-07-16-project-el-enhancements/
(defun project-save-some-buffers (&optional arg)
  "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
  (interactive "P")
  (let* ((project-buffers (project-buffers (project-current)))
         (pred (lambda () (memq (current-buffer) project-buffers))))
    (funcall-interactively #'save-some-buffers arg pred)))

;; Adapted from projectile
(defun project--repeat-until-project-buffer (orig-fun &rest args)
  "Repeat ORIG-FUN with ARGS until the current buffer is a project buffer."
  (if (project-current)
      (let* ((other-project-buffers (make-hash-table :test 'eq))
             (project-buffers (project-buffers (project-current)))
             (max-iterations (length (buffer-list)))
             (counter 0))
        (dolist (buffer project-buffers)
          (unless (eq buffer (current-buffer))
            (puthash buffer t other-project-buffers)))
        (when (cdr-safe project-buffers)
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-project-buffers)))
            (apply orig-fun args)
            (cl-incf counter))))
    (apply orig-fun args)))

(defun project-next-buffer ()
  "In selected window switch to the next project buffer.
If the current buffer does not belong to a project, call `next-buffer'."
  (interactive)
  (project--repeat-until-project-buffer #'next-buffer))

(defun project-previous-buffer ()
  "In selected window switch to the previous project buffer.
If the current buffer does not belong to a project, call `previous-buffer'."
  (interactive)
  (project--repeat-until-project-buffer #'previous-buffer))


(defun project-bin-dev ()
  "Run bin/dev in a compilation buffer."
  (interactive)
  (when-let* ((project (project-current))
              (root (project-root project))
              (path (concat root "bin/dev"))
              (default-directory root))
    (if (file-exists-p path)
        (progn
          (message "Starting bin/dev...")
          (compilation-start path 'compilation-mode
                             (lambda (_arg) "*bin/dev*")))
      (error "%s does not exist!" path))))

(defun project-add-all-in-dir (dir)
  "Recursively add all projects found in DIR to known projects.
Stops descending into a directory once a project is found there."
  (interactive (list (read-directory-name "Parent directory: ")))
  (let ((added 0))
    (cl-labels ((search (d)
                  (if-let ((project (project-current nil d)))
                      (progn
                        (project--remember-dir (project-root project))
                        (cl-incf added))
                    (dolist (subdir (directory-files d t "\\`[^.]"))
                      (when (file-directory-p subdir)
                        (search subdir))))))
      (search (expand-file-name dir)))
    (message "Added %d projects from %s" added dir)))

;; Hide the bin/dev buffer
(add-to-list 'display-buffer-alist
             '("\\*bin/dev\\*" (display-buffer-reuse-window display-buffer-no-window)))

(use-package project
  :ensure nil
  :demand t
  :bind (:prefix-map nyx-project-prefix-map :prefix "C-c p"
                     ("a" . project-persp-find-and-switch)
                     ("p" . project-persp-switch)
                     ("W" . project-persp-switch-workspace)
                     ("P" . project-switch-project)
                     ("w" . project-worktree-switch)
                     ("o" . project-worktree-visit-counterpart)
                     ("O" . project-org-open)
                     ("f" . project-find-file)
                     ("F" . project-or-external-find-file)
                     ("d" . project-find-dir)
                     ("D" . project-dired)
                     ("c" . project-compile)
                     ("C" . project-bin-dev)
                     ("b" . consult-project-buffer)
                     ("r" . project-query-replace-regexp)
                     ("s s" . consult-ripgrep)
                     ("s S" . rg-project)
                     ("C-x s" . project-save-some-buffers))
  :custom ((project-vc-extra-root-markers '(".project" ".projectile"))
           (project-prompter #'project-prompt-project-name)))

(provide 'nyx-project)
;;; nyx-project.el ends here
