;;; nyx-github.el --- GitHub -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Open GitHub/GitLab pages from Emacs
(use-package browse-at-remote
  :ensure t)

;; Interface for creating and managing GitHub gists
(use-package gist
  :ensure t
  :defer t)

(provide 'nyx-github)
;;; nyx-github.el ends here
