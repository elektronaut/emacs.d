;;; nyx-flycheck.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :custom ((flycheck-emacs-lisp-load-path 'inherit)
           (flycheck-temp-prefix ".flycheck"))
  :hook (prog-mode . flycheck-mode)
  :commands flycheck-add-mode
  :config
  (add-to-list 'flycheck-disabled-checkers '(javascript-jshint))
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
  (flycheck-add-mode 'yaml-yamllint 'yaml-mode))

(provide 'nyx-flycheck)
;;; nyx-flycheck.el ends here
