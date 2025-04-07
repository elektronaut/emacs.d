;;; nyx-buffers.el --- Buffers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-persp)

(defun protected-buffer-list ()
  "Return a list of all protected buffers."
  (delq nil (mapcar 'get-buffer
                    '("*scratch*"
                      "*Messages*"
                      "*nyx-tramp-server*"))))

(defun unprotected-buffer-list ()
  "Return a list of all non-protected buffers."
  (seq-difference (buffer-list) (protected-buffer-list)))

(defun kill-other-buffers (&optional arg)
  "Kill all other buffers except the visible ones.
With prefix ARG, also delete other windows."
  (interactive "P")
  (when arg (delete-other-windows))
  (let* ((visible-buffers (mapcar #'window-buffer (window-list)))
         (kill-list (seq-difference (unprotected-buffer-list) visible-buffers)))
    (mapc 'kill-buffer kill-list)
    ;; Clear out unused perspectives.
    (when persp-mode (persp-kill-empty))))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (if persp-mode (persp-switch "none"))
  (delete-other-windows)
  (switch-to-buffer (or (car (protected-buffer-list)) "*scratch*"))
  (kill-other-buffers))

(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-x K" 'kill-other-buffers)

;; Built-in; makes buffer names unique when files have the same name
(use-package uniquify
  :ensure nil
  :custom ((uniquify-buffer-name-style 'forward)
           (uniquify-separator "/")
           (uniquify-after-kill-buffer-p t)
           (uniquify-ignore-buffers-re "^\\*")))

(provide 'nyx-buffers)
;;; nyx-buffers.el ends here
