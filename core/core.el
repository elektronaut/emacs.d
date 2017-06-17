;;; core.el --- Emacs config core
;;; Commentary:
;;; Code:


;;-----------------------------------------------------------------------------
;; Startup
;;-----------------------------------------------------------------------------

(setq load-prefer-newer t
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000)

;; Automatically start server
(require 'server)
(unless (server-running-p) (server-start))


;;-----------------------------------------------------------------------------
;; Macros
;;-----------------------------------------------------------------------------

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))


;;-----------------------------------------------------------------------------
;; Load paths
;;-----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Add Homebrew to the load path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;-----------------------------------------------------------------------------
;; Save files
;;-----------------------------------------------------------------------------

(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

;; Create savefile dir if necessary
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (setq-default save-place t))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))


;;-----------------------------------------------------------------------------
;; Core modules
;;-----------------------------------------------------------------------------

(require 'core-calendar)
(require 'core-compilation)
(require 'core-completion)
(require 'core-crux)
(require 'core-defuns)
(require 'core-dired)
(require 'core-editing)
(require 'core-email)
(require 'core-erc)
(require 'core-flycheck)
(require 'core-flyspell)
(require 'core-git)
(require 'core-global-keys)
(require 'core-helm)
(require 'core-ido)
(require 'core-ivy)
(require 'core-navigation)
(require 'core-packages)
(require 'core-projectile)
(require 'core-rainbow)
(require 'core-slime)
(require 'core-smartparens)
(require 'core-ui)
(require 'core-whitespace)
(require 'core-yank)

(require 'core-modeline) ; Needs to be loaded after core-ui

(when (eq system-type 'darwin)
  (require 'core-macos))

(provide 'core)
;;; core.el ends here
