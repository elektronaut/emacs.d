;;; core-whitespace -- YAML
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'whitespace)

(setq-default indent-tabs-mode nil
              tab-width 8)

(setq whitespace-line-column 80
      whitespace-style '(face tabs empty trailing lines-tail)
      require-final-newline t)

(define-minor-mode core-whitespace-mode
  "Enable automatic whitespace cleanup."
  :global t
  :lighter " cws")

(core-whitespace-mode +1)

(defun core-whitespace-cleanup ()
  "Cleanup whitespace if enabled."
  (if core-whitespace-mode
      (whitespace-cleanup)))

(add-hook 'before-save-hook 'core-whitespace-cleanup)

(provide 'core-whitespace)
;;; core-whitespace ends here
