;;; nyx-emacs-lisp.el --- Emacs Lisp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-lisp)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(use-package elisp-slime-nav
  :ensure t
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (ielm-mode       . elisp-slime-nav-mode)))

(add-hook 'ielm-mode-hook
          (lambda ()
            (run-hooks 'nyx-lisp-interactive-hook)
            (eldoc-mode +1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (run-hooks 'nyx-lisp-hook)
            (eldoc-mode +1)
            (rainbow-mode +1)
            (setq mode-name "EL")))

(provide 'nyx-emacs-lisp)
;;; nyx-emacs-lisp.el ends here
