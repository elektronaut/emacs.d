;;; nyx-dired.el --- Dired -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-diff)

(use-package dired
  :ensure nil
  :after (diff-hl)
  :custom ((dired-use-ls-dired t)
           (dired-recursive-deletes 'always)
           (dired-recursive-copies 'always)
           (dired-dwim-target t)
           (dired-listing-switches "-aBhl"))
  :hook ((dired-mode . diff-hl-dired-mode)
         (dired-mode . dired-hide-details-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil
  :after (dired)
  :hook ((dired-mode . dired-omit-mode))
  :custom ((dired-omit-verbose nil))
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.bundle$\\|^.git$\\|^.DS_Store$\\|^.project$\\|^.projectile$"))
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(provide 'nyx-dired)
;;; nyx-dired.el ends here
