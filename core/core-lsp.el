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
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

;; (use-package lsp-mode
;;   :ensure t
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
;;   :ensure t
;;   :commands lsp-ui-mode)

(provide 'core-lsp)
;;; core-lsp.el ends here
