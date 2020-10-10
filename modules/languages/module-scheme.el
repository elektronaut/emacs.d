;;; module-scheme -- Scheme config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-lisp)

;; MIT Scheme path
(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/MIT-Scheme.app/Contents/Resources")

(use-package geiser
  :defer 10
  :init
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename (expand-file-name
                                      "geiser-history" savefile-dir)))

(add-hook 'scheme-mode-hook (lambda () (run-hooks 'module-lisp-hook)))

(provide 'module-scheme)
;;; module-scheme ends here
