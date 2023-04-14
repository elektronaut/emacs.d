;;; core-navigation -- Navigation
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package ag
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-char)
         ("C-c J" . avy-goto-word-or-subword-1)
         ("s-."   . avy-goto-word-or-subword-1))
  :init
  (setq avy-background t
        avy-style 'at-full))

(use-package browse-at-remote
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package etags-select
  :ensure nil
  :bind (("C-x t" . etags-select-find-tag-at-point)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))

(use-package project-explorer
  :ensure t
  :bind (("C-c p x" . project-explorer-open)))

(use-package rg
  :ensure t
  :bind (("C-c p s S" . rg-project)))

(use-package deadgrep
  :ensure t)

;; (use-package wgrep-ag
;;   :ensure t
;;   :bind (("C-c p s S" . projectile-ag)))

(provide 'core-navigation)
;;; core-navigation ends here
