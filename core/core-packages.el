;;; core-packages.el --- Configuration for packages
;;; Commentary:
;;; Code:

;;-----------------------------------------------------------------------------
;; Built-ins
;;-----------------------------------------------------------------------------

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eshell
  :ensure nil
  :bind (("C-x m" . eshell))
  :init
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir))
  :config
  ;; Start a new eshell even if one is active.
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))))

;; Clean up obsolete buffers automatically
(use-package midnight :ensure nil)

(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh"))


;;-----------------------------------------------------------------------------
;; Extra packages
;;-----------------------------------------------------------------------------

(use-package anzu
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package discover-my-major
  :bind (("C-c m" . discover-my-major)))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode +1))

(provide 'core-packages)
;;; core-packages.el ends here
