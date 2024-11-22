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

(use-package project
  :ensure t
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
                     ("b" . consult-project-buffer)
                     ("r" . project-query-replace-regexp)
                     ("s s" . consult-ripgrep)
                     ("s S" . rg-project)
                     ("C-x s" . project-save-some-buffers)))

(provide 'nyx-project)
;;; nyx-project.el ends here
