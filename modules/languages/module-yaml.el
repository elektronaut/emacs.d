;;; module-yaml -- YAML
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package yaml-mode
  :mode "\\.yaml\\'" "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (whitespace-mode +1)
              (subword-mode +1)
              (add-hook 'before-save-hook
                        'core-whitespace-cleanup nil t))))

(provide 'module-yaml)
;;; module-yaml ends here
