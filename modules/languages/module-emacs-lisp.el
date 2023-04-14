;;; module-emacs-lisp -- Emacs lisp
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-lisp)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(add-hook 'ielm-mode-hook
          (lambda ()
            (run-hooks 'module-lisp-interactive-hook)
            (eldoc-mode +1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (run-hooks 'module-lisp-hook)
            (eldoc-mode +1)
            (rainbow-mode +1)
            (setq mode-name "EL")))

(provide 'module-emacs-lisp)
;;; module-emacs-lisp ends here
