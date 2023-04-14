;;; core-flycheck -- Flycheck
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck"
                flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                   '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
  (flycheck-add-mode 'yaml-yamllint 'yaml-mode)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'core-flycheck)
;;; core-flycheck ends here
