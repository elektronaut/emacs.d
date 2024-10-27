;;; core-lsp -- LSP mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil)

  (dolist (dir '("[/\\\\]\\tmp"
                 "[/\\\\]\\coverage"
                 "[/\\\\]\\db/dis"))
    (push dir lsp-file-watch-ignored-directories)))

(provide 'core-lsp)
;;; core-lsp.el ends here
