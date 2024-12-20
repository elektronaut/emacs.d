;;; nyx-marginalia.el --- Marginalia -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (;;("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-m" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(provide 'nyx-marginalia)
;;; nyx-marginalia.el ends here
