;;; core-shell -- Shell
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package eshell
  :ensure nil
  :bind (("C-x m" . eshell))
  :init
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir))
  :config
  ;; Start a new eshell even if one is active.
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))))

(use-package multi-term
  :bind (("C-c t" . multi-term-dedicated-toggle)))

(provide 'core-shell)
;;; core-shell ends here
