;;; core-gcmh -- GCMH
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package gcmh
  :ensure t
  :init
  (setq-default gcmh-idle-delay 5
                gcmh-high-cons-threshold (* 32 1024 1024)  ; 32mb
                gcmh-verbose nil)
  :config
  (gcmh-mode))

(provide 'core-gcmh)
;;; core-gcmh ends here
