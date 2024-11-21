;;; nyx-clojure.el --- Clojure -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-lisp)

(defun nyx-clojure-defaults ()
  "Defaults for clojure buffers."
  (subword-mode +1)
  (run-hooks 'nyx-lisp-hook))

(defun nyx-clojure-interactive-defaults ()
  "Defaults for interactive clojure sessions."
  (subword-mode +1)
  (run-hooks 'nyx-lisp-interactive-hook))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :hook (clojure-mode . nyx-clojure-defaults))

(use-package flycheck-clojure
  :ensure t
  :after (flycheck clojure-mode))

(use-package cider
  :ensure t
  :defer t
  :custom (nrepl-log-messages t)
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . nyx-clojure-interactive-defaults)))

(provide 'nyx-clojure)
;;; nyx-clojure.el ends here
