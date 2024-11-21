;;; nyx-scheme.el --- Scheme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-savefiles)
(require 'nyx-lisp)

;; MIT Scheme path
(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/MIT-Scheme.app/Contents/Resources")

;; Geiser is a generic Emacs/Scheme interaction mode, featuring an
;; enhanced REPL and a set of minor modes improving Emacs’ basic
;; scheme major mode.
(use-package geiser
  :ensure t
  :defer t
  :custom
  (geiser-mode-start-repl-p t)
  (geiser-repl-history-filename (expand-file-name
                                 "geiser-history" savefile-dir)))

(add-hook 'scheme-mode-hook (lambda () (run-hooks 'nyx-lisp-hook)))

(provide 'nyx-scheme)
;;; nyx-scheme.el ends here
