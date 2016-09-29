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

;; Commented out to make package.el happy.
; (package-initialize)

;;; init.el ends here
