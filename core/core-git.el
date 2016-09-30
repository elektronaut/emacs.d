;;; core-git.el --- Settings for git
;;; Commentary:
;;; Code:

(use-package gist)

(use-package git-timemachine)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("s-m m"   . magit-status)
         ("s-m l"   . magit-log)
         ("s-m f"   . magit-log-buffer-file)
         ("s-m b"   . magit-blame))
  :init
  (setq-default magit-use-overlays nil))

(provide 'core-git)
;;; core-git.el ends here
