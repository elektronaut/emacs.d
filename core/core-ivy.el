;;; core-ivy -- Ivy config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'thingatpt)

(require 'core-projectile)

(use-package counsel
  :config
  (use-package counsel-projectile)
  (use-package counsel-dash
    :defer 15))

(use-package swiper)

(use-package ivy
  :bind (:map ivy-minibuffer-map
         ("C-m"       . ivy-alt-done)
         :map ivy-mode-map
         ("\C-s"      . swiper)
         ("C-S-s"     . swiper-at-point)
         ("M-x"       . counsel-M-x)
         ("C-x b"     . ivy-switch-buffer)
         ("C-x C-f"   . counsel-find-file)
         ("C-c f"     . counsel-recentf)
         ("M-y"       . counsel-yank-pop)
         :map projectile-mode-map
         ("C-c p s s" . counsel-projectile-ag))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq projectile-completion-system 'ivy)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1)
  (defun swiper-at-point ()
    "Search for thing at point"
    (interactive)
    (swiper (thing-at-point 'symbol)))
  (defun counsel-projectile-ag ()
    "Search in projectile root"
    (interactive)
    (counsel-ag (thing-at-point 'symbol) (projectile-project-root))))

(use-package smex
  :init
  (setq smex-save-file (expand-file-name "smex-items" savefile-dir))
  :config
  (smex-initialize))

(provide 'core-ivy)
;;; core-ivy ends here
