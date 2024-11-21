;;; nyx-coffee.el --- Coffeescript -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :custom (coffee-tab-width 2)
  :hook (coffee-mode . subword-mode)
  :functions (coffee-compiled-file-name coffee-cos-mode)
  :config
  (add-to-list 'coffee-args-compile "--no-header")

  ;; Compile on save if the compiled file exists
  (add-hook 'coffee-mode-hook
            (lambda ()
              (and (buffer-file-name)
                   (file-exists-p (buffer-file-name))
                   (file-exists-p (coffee-compiled-file-name (buffer-file-name)))
                   (coffee-cos-mode t)))))

(provide 'nyx-coffee)
;;; nyx-coffee.el ends here
