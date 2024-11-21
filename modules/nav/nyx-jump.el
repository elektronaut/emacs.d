;;; nyx-jump.el --- Jump -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Jump to definitions without tags files
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Interface for selecting from multiple tags
(use-package etags-select
  :ensure nil
  :bind (("C-x t" . etags-select-find-tag-at-point)))

(provide 'nyx-jump)
;;; nyx-jump.el ends here
