;;; nyx-whitespace.el --- Whitespace -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'whitespace)

(setq-default indent-tabs-mode nil
              tab-width 8
              tab-always-indent 'complete
              require-final-newline t
              whitespace-line-column 80
              whitespace-style '(face tabs empty trailing lines-tail))

(delete-selection-mode t)
(global-auto-revert-mode t)

;; Built-in package for handling tabs and spaces
(use-package tabify
  :ensure nil
  :config)

(provide 'nyx-whitespace)
;;; nyx-whitespace.el ends here
