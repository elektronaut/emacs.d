;;; nyx-undo-tree.el --- Undo tree -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package undo-tree
  :ensure t
  :custom ((undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
           (undo-tree-auto-save-history t))
  :config
  (global-undo-tree-mode))

(provide 'nyx-undo-tree)
;;; nyx-undo-tree.el ends here
