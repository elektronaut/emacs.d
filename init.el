;;; init.el --- Emacs config entry
;;
;;; Commentary:
;;; License: MIT

;;; Code:

(defconst emacs-start-time (current-time))

(setq-default user-full-name    "Inge Jørgensen"
              user-mail-address "inge@elektronaut.no")

;; Bootstrap load paths
(defvar emacs-root-dir (file-name-directory load-file-name)
  "The Emacs config dir.")
(add-to-list 'load-path (expand-file-name "core" emacs-root-dir))
(add-to-list 'load-path (expand-file-name "modules" emacs-root-dir))

;; Core
(require 'core)

;; Features
(require 'module-company)
(require 'module-crux)
(require 'module-erc)
(require 'module-helm)
(require 'module-ido)
(require 'module-ivy)
(require 'module-python)
(require 'module-smartparens)

;; Language support
(require 'module-c)
(require 'module-clojure)
(require 'module-common-lisp)
(require 'module-css)
(require 'module-elixir)
(require 'module-elm)
(require 'module-emacs-lisp)
(require 'module-go)
(require 'module-haskell)
(require 'module-js)
(require 'module-markdown)
(require 'module-org)
(require 'module-perl)
(require 'module-puppet)
(require 'module-ruby)
(require 'module-scala)
(require 'module-scheme)
(require 'module-shell)
(require 'module-xml)
(require 'module-yaml)
(require 'module-web)

;; Load custom settings
(setq custom-file (expand-file-name "custom.el" emacs-root-dir))
(load custom-file)

(message "Init complete in %.2f seconds"
         (- (time-to-seconds (current-time)) (time-to-seconds emacs-start-time)))

;; Commented out to make package.el happy.
; (package-initialize)

;;; init.el ends here
