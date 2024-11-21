;;; nyx-scala.el --- Scala -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'"
  :hook ((scala-mode . subword-mode)))

(provide 'nyx-scala)
;;; nyx-scala.el ends here
