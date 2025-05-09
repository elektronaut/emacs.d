;;; nyx-vertico.el --- Vertico -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :ensure t
  :custom
  (vertico-count 15)
  :init
  (vertico-mode))

(provide 'nyx-vertico)
;;; nyx-vertico.el ends here
