;;; nyx-term.el --- Terminal emulators -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-savefiles)

(use-package eshell
  :ensure nil
  :defer t
  :bind (("C-x m" . eshell))
  :custom (eshell-directory-name (expand-file-name "eshell" savefile-dir)))

(use-package multi-term
  :ensure t
  :defer t)

(use-package vterm
  :ensure t
  :defer t)

(use-package multi-vterm
  :defer t
  :ensure t
  :bind (("C-c t" . multi-vterm-dedicated-toggle)))

(provide 'nyx-term)
;;; nyx-term.el ends here
