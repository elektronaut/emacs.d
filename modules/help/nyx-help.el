;;; nyx-help.el --- Help -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(use-package discover-my-major
  :ensure t
  :bind (("C-c m" . discover-my-major)))

(provide 'nyx-help)
;;; nyx-help.el ends here
