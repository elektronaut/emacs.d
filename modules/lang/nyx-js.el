;;; nyx-js.el --- Javascript -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-treesitter)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'" "\\.tsx\\'")

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'" "\\.pac\\'"
  :custom ((js-basic-offset 2)
           (js-indent-level 2))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js2-mode-hook
            (lambda ()
              ;; Make electric-layout-mode play nice with smartparens
              (setq-local electric-layout-rules '((?\; . after)))
              (setq mode-name "JS2")
              (js2-imenu-extras-mode +1))))

(provide 'nyx-js)
;;; nyx-js.el ends here
