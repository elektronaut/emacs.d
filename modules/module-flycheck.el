;;; module-flycheck -- Flycheck
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck"
                flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                   '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'module-flycheck)
;;; module-flycheck ends here
