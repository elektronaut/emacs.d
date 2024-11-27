;;; nyx-lsp.el --- LSP mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :custom ((lsp-headerline-breadcrumb-enable nil)
           (lsp-auto-execute-action nil)
           (lsp-modeline-code-actions-segment '(count))
           (lsp-keymap-prefix "C-c l"))
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :commands (lsp)
  :config
  (dolist (dir '("[/\\\\]\\tmp"
                 "[/\\\\]\\coverage"
                 "[/\\\\]\\db/dis"))
    (push dir lsp-file-watch-ignored-directories)))

(provide 'nyx-lsp)
;;; nyx-lsp.el ends here
