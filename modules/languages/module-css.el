;;; module-css -- CSS config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(defun module-css-defaults ()
  (rainbow-mode +1))

(use-package css-mode
  :ensure nil
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2
        css-fontify-colors nil)
  (add-hook 'css-mode-hook 'module-css-defaults)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.postcss\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.pcss\\'" . scss-mode)))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

;; scss-mode is now built-in
;(use-package scss-mode
;  :mode "\\.scss\\'" "\\.scss\\.erb\\'"
;  :init
;  (setq-default scss-compile-at-save nil
;                scss-sass-command "~/.rbenv/shims/sass")
;  (add-hook 'scss-mode-hook 'module-css-defaults))

(provide 'module-css)
;;; module-css ends here
