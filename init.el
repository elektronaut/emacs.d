;;; init.el --- Emacs config entry -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; License: MIT

;;; Code:

(setopt user-full-name    "Inge Jørgensen"
        user-mail-address "inge@elektronaut.no")


;;-----------------------------------------------------------------------------
;; Startup
;;-----------------------------------------------------------------------------

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2fs seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setopt load-prefer-newer t
        large-file-warning-threshold 100000000)


;;-----------------------------------------------------------------------------
;; Packages and paths
;;-----------------------------------------------------------------------------

;; Initialize elpaca
(require 'nyx-elpaca (concat user-emacs-directory "nyx-elpaca.el"))

;; Configure use-package
(require 'use-package)
(setopt use-package-compute-statistics t)

;; Keep .emacs.d clean
(use-package no-littering
  :ensure (:wait t)
  :demand t)

;; Enable auto-compile
(use-package auto-compile
  :ensure (:wait t)
  :custom ((auto-compile-display-buffer nil)
           (auto-compile-mode-line-counter t))
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Required third-party packages
(use-package dash
  :ensure (:wait t)
  :demand t)


;;-----------------------------------------------------------------------------
;; Server
;;-----------------------------------------------------------------------------

;; Automatically start server
(require 'server)
(setopt server-port "12345")
(unless (server-running-p) (server-start))


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
(setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file)

(setopt gc-cons-threshold (* 16 1024 1024)) ;; 16mb

;;; init.el ends here
