;;; module-common-lisp -- Common Lisp
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-lisp)
(require 'module-slime)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(add-hook 'lisp-mode-hook 'module-lisp-defaults)

(provide 'module-common-lisp)
;;; module-common-lisp ends here
