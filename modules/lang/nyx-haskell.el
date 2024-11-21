;;; nyx-haskell.el --- Haskell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :hook ((haskell-mode . subword-mode)
         (haskell-mode . eldoc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode)))

(provide 'nyx-haskell)
;;; nyx-haskell.el ends here
