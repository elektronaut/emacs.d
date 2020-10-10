;;; module-haskell -- Haskell
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (subword-mode +1)
              (eldoc-mode +1)
              (haskell-indentation-mode +1)
              (interactive-haskell-mode +1))))

(provide 'module-haskell)
;;; module-haskell ends here
