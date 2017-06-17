;;; core-dired -- Dired
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)
  (use-package dired-x
    :ensure nil
    :config
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.bundle$\\|^.git$\\|^.DS_Store$\\|^.projectile$"))
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

(provide 'core-dired)
;;; core-dired ends here
