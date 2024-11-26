;;; nyx-savefiles.el --- Savefiles -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'no-littering)

;; Disable desktop save mode
(desktop-save-mode 0)

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
        savehist-autosave-interval 60)
  (savehist-mode +1))

;; Remember and restore cursor position in files
;; (use-package saveplace
;;   :ensure nil
;;   :custom ((save-place-mode t)))

;; Track recently opened files
(use-package recentf
  :ensure nil
  :functions (recentf-expand-file-name)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (recentf-mode +1))

(provide 'nyx-savefiles)
;;; nyx-savefiles.el ends here
