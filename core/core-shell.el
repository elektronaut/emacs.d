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

(provide 'core-shell)
;;; core-shell ends here
