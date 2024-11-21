;;; nyx-go.el --- Go -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-lsp)

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred))
  :functions lsp-go-install-save-hooks
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(provide 'nyx-go)
;;; nyx-go.el ends here
