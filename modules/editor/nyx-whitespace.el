;;; nyx-whitespace.el --- Whitespace -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'whitespace)

(setopt indent-tabs-mode nil
        tab-width 8
        tab-always-indent 'complete
        require-final-newline t
        standard-indent 2
        whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))

(delete-selection-mode t)
(global-auto-revert-mode t)

(define-minor-mode nyx-whitespace-mode
  "Enable automatic whitespace cleanup."
  :global t
  :group 'nyx
  :lighter " nws")

(nyx-whitespace-mode +1)

(defun nyx-whitespace-cleanup ()
  "Cleanup whitespace if enabled."
  (when (and nyx-whitespace-mode
             (derived-mode-p 'prog-mode))
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'nyx-whitespace-cleanup)

;; Built-in package for handling tabs and spaces
(use-package tabify
  :ensure nil
  :config)

(provide 'nyx-whitespace)
;;; nyx-whitespace.el ends here
