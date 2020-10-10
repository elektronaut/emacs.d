;;; module-c -- C
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r"
                  c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (whitespace-toggle-options '(tabs))
            (setq indent-tabs-mode t)))

(provide 'module-c)
;;; module-c ends here
