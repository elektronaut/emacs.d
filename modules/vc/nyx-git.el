;;; nyx-git.el --- Git -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Step through git history of a file
(use-package git-timemachine
  :ensure t)

;; Major modes for various git-related files (.gitignore, etc)
(use-package git-modes
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(provide 'nyx-git)
;;; nyx-git.el ends here
