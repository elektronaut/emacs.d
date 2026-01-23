;;; nyx-project.el --- Project -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-consult)

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
                     ("P" . project-switch-project)
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
  :custom ((project-vc-extra-root-markers '(".project" ".projectile"))))

(provide 'nyx-project)
;;; nyx-project.el ends here
