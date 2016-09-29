;;; module-ivy -- Ivy config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'thingatpt)

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("C-m"       . ivy-alt-done)
              :map ivy-mode-map
              ("\C-s"      . swiper)
              ("M-x"       . counsel-M-x)
              ("C-x b"     . ivy-switch-buffer)
              ("C-x C-b"   . ivy-switch-buffer)
              ("C-x C-f"   . counsel-find-file)
              ("C-c f"     . counsel-recentf)
              ("M-y"       . counsel-yank-pop)
              ("C-c p s s" . counsel-projectile-ag))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq projectile-completion-system 'ivy)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1)
  (defun counsel-projectile-ag ()
    "Search in projectile root"
    (interactive)
    (counsel-ag (thing-at-point 'symbol) (projectile-project-root))))

(provide 'module-ivy)
;;; module-ivy ends here
