;;; nyx-editorconfig.el --- Editorconfig -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Maintains consistent coding styles across editors and IDEs
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(provide 'nyx-editorconfig)
;;; nyx-editorconfig.el ends here
