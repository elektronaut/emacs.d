;;; module-js -- Javascript
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package js2-mode
  :mode "\\.js\\'" "\\.pac\\'"
  :init
  (setq-default js-basic-offset 2)
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (defun my-js2-mode-hook ()
    ;; Make electric-layout-mode play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package rjsx-mode
  :mode "\\.jsx\\'")

(provide 'module-js)
;;; module-js ends here
