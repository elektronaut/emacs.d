;;; init.el --- Emacs config entry
;;
;;; Commentary:
;;; License: MIT

;;; Code:

(defconst emacs-start-time (current-time))

(setq-default user-full-name    "Inge Jørgensen"
              user-mail-address "inge@elektronaut.no")

;; Bootstrap load paths
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Enable packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Core
(require 'core)

;; Modules
(require 'module-c)
(require 'module-clojure)
(require 'module-coffee)
(require 'module-common-lisp)
(require 'module-css)
(require 'module-elixir)
(require 'module-elm)
(require 'module-emacs-lisp)
(require 'module-go)
(require 'module-haskell)
(require 'module-js)
(require 'module-markdown)
(require 'module-misc)
(require 'module-org)
(require 'module-perl)
(require 'module-puppet)
(require 'module-python)
(require 'module-ruby)
(require 'module-scala)
(require 'module-scheme)
(require 'module-shell)
(require 'module-web)
(require 'module-xml)
(require 'module-yaml)

;; Load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(message "Init complete in %.2f seconds"
         (- (time-to-seconds (current-time)) (time-to-seconds emacs-start-time)))

;;; init.el ends here
