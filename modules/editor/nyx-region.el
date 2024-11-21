;;; nyx-region.el --- Region -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Semantic region expansion based on syntax and delimiters
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(provide 'nyx-region)
;;; nyx-region.el ends here
