;;; nyx-apheleia.el --- Apheleia -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Run code formatter on buffer contents without moving point, using
;;;   RCS patches and dynamic programming.

;;; Code:

(use-package apheleia
  :ensure t
  :functions (apheleia-global-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(ruby-mode . rubocop))
  (add-to-list 'apheleia-mode-alist '(ruby-ts-mode . rubocop))
  (apheleia-global-mode +1))

(provide 'nyx-apheleia)
;;; nyx-apheleia.el ends here
