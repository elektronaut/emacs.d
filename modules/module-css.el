;;; module-css -- CSS config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(defun module-css-defaults ()
  (rainbow-mode +1))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook 'module-css-defaults))

(use-package less-css-mode :mode "\\.less\\'")

(use-package sass-mode :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'" "\\.scss\\.erb\\'"
  :init
  (setq-default scss-compile-at-save nil)
  (add-hook 'scss-mode-hook 'module-css-defaults))

(provide 'module-css)
;;; module-css ends here
