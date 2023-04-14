;;; module-js -- Javascript
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-treesitter)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'" "\\.tsx\\'")

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'" "\\.pac\\'"
  :init
  (setq-default js-basic-offset 2
                js-indent-level 2)
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (defun my-js2-mode-hook ()
    ;; Make electric-layout-mode play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

;; (use-package rjsx-mode
;;   :ensure t
;;   :mode "\\.jsx\\'" "\\.tsx\\'")

(provide 'module-js)
;;; module-js ends here
