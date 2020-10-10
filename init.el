;;; init.el --- Emacs config entry
;;
;;; Commentary:
;;; License: MIT

;;; Code:

(setq-default user-full-name    "Inge Jørgensen"
              user-mail-address "inge@elektronaut.no")


;;-----------------------------------------------------------------------------
;; Startup
;;-----------------------------------------------------------------------------

; Defer garbage collection for now. gcmh-mode takes care of this later.
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

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; Enable packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Enable use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics nil
      use-package-verbose nil)

;; Add Homebrew to the load path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set and create savefile dir
(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))


;;-----------------------------------------------------------------------------
;; Load configuration
;;-----------------------------------------------------------------------------

(defun init-dir (dir)
  "Add DIR to load path and require all files within."
  (let ((fullpath (expand-file-name dir user-emacs-directory)))
    (add-to-list 'load-path fullpath)
    (mapc (lambda (name)
            (require (intern (file-name-sans-extension name))))
          (directory-files fullpath nil "\\.elc?$"))))

(init-dir "core")
(init-dir "modules")

;; Load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
