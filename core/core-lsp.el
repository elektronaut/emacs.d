;;; core-lsp -- LSP mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((js2-mode . lsp)
;;          (css-mode . lsp)
;;          (scss-mode . lsp)
;;          (python-mode . lsp)
;;          ;;(ruby-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

(provide 'core-lsp)
;;; core-lsp.el ends here
