;;; nyx-common-lisp.el --- Common Lisp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-lisp)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(add-hook 'lisp-mode-hook 'nyx-lisp-defaults)

(provide 'nyx-common-lisp)
;;; nyx-common-lisp.el ends here
