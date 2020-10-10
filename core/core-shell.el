;;; core-shell -- Shell
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package eshell
  :ensure nil
  :defer t
  :bind (("C-x m" . eshell))
  :init
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir))
  :config
  ;; Start a new eshell even if one is active.
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))))

(use-package multi-term
  :ensure t
  :defer t)

(use-package vterm
  :ensure t
  :defer t)

(use-package multi-vterm
  :defer t
  :ensure t
  :bind (("C-c t" . multi-vterm-dedicated-toggle)))

(provide 'core-shell)
;;; core-shell ends here
