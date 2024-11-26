;;; nyx-magit.el --- Magit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-diff)

;; Complete git interface for Emacs
(use-package magit
  :ensure t
  :after (diff-hl)
  :demand t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom ((magit-use-overlays nil)
           (magit-commit-diff-inhibit-same-window t)
           (magit-repository-directories
            '(("~/Dev/anyone/apps" . 1)
              ("~/Dev/gems" . 1))))
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)))

;; Extends magit with GitHub/GitLab integration
(use-package forge
  :ensure t
  :after (magit)
  :defer t)

(provide 'nyx-magit)
;;; nyx-magit.el ends here
