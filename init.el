;;; init.el --- Emacs config entry -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; License: MIT

;;; Code:

(setq-default user-full-name    "Inge Jørgensen"
              user-mail-address "inge@elektronaut.no")


;;-----------------------------------------------------------------------------
;; Startup
;;-----------------------------------------------------------------------------

;; Defer garbage collection for now. gcmh-mode takes care of this later.
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2fs seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq load-prefer-newer t
      large-file-warning-threshold 100000000)

;; Automatically start server
(require 'server)
(setq server-port 12345)
(unless (server-running-p) (server-start))


;;-----------------------------------------------------------------------------
;; Packages and paths
;;-----------------------------------------------------------------------------

;; Enable packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Enable use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose nil)

;; Enable auto-compile
(use-package auto-compile
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


;;-----------------------------------------------------------------------------
;; Load configuration
;;-----------------------------------------------------------------------------

(defun init-dirs (dirs)
  "Add DIRS to load path, then require all files within."
  (let ((paths (mapcar (lambda (dir) (expand-file-name dir user-emacs-directory)) dirs)))
    (mapc (lambda (path) (add-to-list 'load-path path)) paths)
    (mapc (lambda (path)
            (mapc (lambda (file)
                    (require (intern (file-name-sans-extension file))))
                  (directory-files path nil "\\.elc?$"))) paths)))

(defun init-all-dirs (dirname)
  "Run init-dirs on all subdirs in DIRNAME."
  (let* ((dir (expand-file-name dirname user-emacs-directory))
         (subdirs (cl-remove-if-not #'file-directory-p (directory-files dir t "^[^.]")))
         (relative-dirs (mapcar
                         (lambda (d) (concat dirname "/" (file-name-nondirectory d)))
                         subdirs)))
    (init-dirs relative-dirs)))

(init-all-dirs "modules")

;; Load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
