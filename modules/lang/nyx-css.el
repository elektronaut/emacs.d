;;; nyx-css.el --- CSS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'"     . scss-mode)
         ("\\.postcss\\'" . scss-mode)
         ("\\.pcss\\'"    . scss-mode)
         ("\\.scss\\'"    . scss-mode))
  :custom ((css-indent-offset 2)
           (css-fontify-colors nil))
  :hook ((css-mode  . rainbow-mode)
         (scss-mode . rainbow-mode)))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(provide 'nyx-css)
;;; nyx-css.el ends here
