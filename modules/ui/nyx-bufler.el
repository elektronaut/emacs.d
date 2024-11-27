;;; nyx-bufler.el --- Bufler -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bufler
  :ensure t
  :bind (("C-x b" . bufler-switch-buffer)
         ("C-x C-b" . bufler)))

(provide 'nyx-bufler)
;;; nyx-bufler.el ends here
