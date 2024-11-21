;;; nyx-diff.el --- Diff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Built-in diff tool for comparing files/buffers interactively
(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Shows git diff indicators in the gutter/fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1))

(provide 'nyx-diff)
;;; nyx-diff.el ends here
