;;; nyx-ripgrep.el --- Ripgrep -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Interface for ripgrep search tool
(use-package rg
  :ensure t
  :bind (("C-c p s S" . rg-project)))

(provide 'nyx-ripgrep)
;;; nyx-ripgrep.el ends here
