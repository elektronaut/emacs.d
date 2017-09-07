;;; core-navigation -- Navigation
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package ag)

(use-package avy
  :bind (("C-c j" . avy-goto-char)
         ("C-c J" . avy-goto-word-or-subword-1)
         ("s-."   . avy-goto-word-or-subword-1))
  :init
  (setq avy-background t
        avy-style 'at-full))

(use-package browse-at-remote)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package etags-select
  :ensure nil
  :bind (("C-x t" . etags-select-find-tag-at-point)))

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)))

(use-package project-explorer
  :bind (("C-c p x" . project-explorer-open)))

(use-package wgrep-ag
  :bind (("C-c p s S" . projectile-ag)))

(provide 'core-navigation)
;;; core-navigation ends here
