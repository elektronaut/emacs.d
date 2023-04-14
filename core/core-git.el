;;; core-git.el --- Settings for git
;;; Commentary:
;;; Code:

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t)

(use-package git-modes
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
	       (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  (setq-default
   magit-use-overlays nil
   magit-repository-directories '(("~/Dev/anyone/apps" . 1)
                                  ("~/Dev/gems" . 1))))

(use-package forge
  :ensure t
  :defer t)

(provide 'core-git)
;;; core-git.el ends here
