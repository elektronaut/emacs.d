;;; core-tramp -- Tramp
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh"))

(provide 'core-tramp)
;;; core-tramp ends here
