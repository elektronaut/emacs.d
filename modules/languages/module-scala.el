;;; module-scala -- Scala
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'"
  :config
  (add-hook 'scala-mode-hook 'subword-mode)
  ;; (use-package ensime
  ;;   :config
  ;;   (add-hook 'scala-mode-hook 'ensime-mode))
  )

(provide 'module-scala)
;;; module-scala ends here
