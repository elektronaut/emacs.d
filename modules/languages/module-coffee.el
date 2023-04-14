;;; module-coffee -- Coffeescript
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :init
  (setq-default coffee-tab-width 2)
  :config
  (add-hook 'coffee-mode-hook 'subword-mode)
  (defun my-coffee-mode-hook ()
    ;; Compile on save if the compiled file exists
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name (buffer-file-name)))
         (coffee-cos-mode t)))
  (add-hook 'coffee-mode-hook 'my-coffee-mode-hook)
  (add-to-list 'coffee-args-compile "--no-header"))

(provide 'module-coffee)
;;; module-coffee ends here
