;;; core-git.el --- Settings for git
;;; Commentary:
;;; Code:

(use-package gist
  :defer t)

(use-package git-timemachine)

(use-package git-modes
  :defer t
  :init
  (add-to-list 'auto-mode-alist
	       (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package magit
  :defer 30
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("s-m m"   . magit-status)
         ("s-m l"   . magit-log)
         ("s-m f"   . magit-log-buffer-file)
         ("s-m b"   . magit-blame))
  :init
  (setq-default
   magit-use-overlays nil
   magit-repository-directories '(("~/Dev/anyone/apps" . 1)
                                  ("~/Dev/gems" . 1))))



(use-package forge
  :after magit)

(provide 'core-git)
;;; core-git.el ends here
