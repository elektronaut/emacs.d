;;; module-lisp -- Lisp config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-rainbow)

(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(defun module-lisp-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq module-lisp-hook 'module-lisp-defaults)

;; interactive modes don't need whitespace checks
(defun module-lisp-interactive-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq module-lisp-interactive-hook 'module-lisp-interactive-defaults)

(provide 'module-lisp)
;;; module-lisp ends here
