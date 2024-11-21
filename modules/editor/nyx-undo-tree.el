;;; nyx-undo-tree.el --- Undo tree -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(provide 'nyx-undo-tree)
;;; nyx-undo-tree.el ends here
