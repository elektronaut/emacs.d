;;; nyx-treesitter.el --- Tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Automatic installation, usage, and fallback for tree-sitter
;; major modes.
(use-package treesit-auto
  :ensure t
  :demand t
  :commands (global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'nyx-treesitter)
;;; nyx-treesitter.el ends here
