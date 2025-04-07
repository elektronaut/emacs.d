;;; nyx-orderless.el --- Orderless -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :ensure t
  :init
  :custom ((completion-styles '(orderless basic))
           (completion-category-defaults nil)
           (completion-category-overrides '((file (styles partial-completion))))))

(provide 'nyx-orderless)
;;; nyx-orderless.el ends here
