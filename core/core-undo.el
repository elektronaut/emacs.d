;;; core-undo -- Undo
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(provide 'core-undo)
;;; core-undo ends here
