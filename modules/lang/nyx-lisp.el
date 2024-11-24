;;; nyx-lisp.el --- Lisp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar nyx-lisp-hook nil
  "Hook for Lisp buffers.")

(defvar nyx-lisp-interactive-hook nil
  "Hook for interactive Lisp buffers.")

(keymap-set read-expression-map "TAB" 'completion-at-point)

(defun nyx-lisp-defaults ()
  "Set up default configuration for Lisp modes.
Enables strict parenthesis matching and rainbow delimiters."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(defun nyx-lisp-interactive-defaults ()
  "Set up configuration for interactive Lisp modes.
Enables strict parenthesis matching and rainbow delimiters,
but disables whitespace mode."
  (nyx-lisp-defaults)
  ;; interactive modes don't need whitespace checks
  (whitespace-mode -1))

(add-hook 'nyx-lisp-hook 'nyx-lisp-defaults)
(add-hook 'nyx-lisp-interactive-hook 'nyx-lisp-interactive-defaults)

(provide 'nyx-lisp)
;;; nyx-lisp.el ends here
