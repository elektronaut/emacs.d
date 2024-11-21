;;; nyx-savefiles.el --- Savefiles -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable desktop save mode
(desktop-save-mode 0)

;; Set and create savefile dir
(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; Autosave/backups
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      delete-by-moving-to-trash t)

;; Save minibuffer history between sessions
(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

;; Remember and restore cursor position in files
(use-package saveplace
  :ensure nil
  :custom ((save-place t)
           (save-place-file (expand-file-name "saveplace" savefile-dir))))

;; Track recently opened files
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(provide 'nyx-savefiles)
;;; nyx-savefiles.el ends here
