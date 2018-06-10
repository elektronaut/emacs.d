;;; module-clojure -- Clojure
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-lisp)

(defun module-clojure-defaults ()
  (subword-mode +1)
  (run-hooks 'module-lisp-hook))

(defun module-clojure-interactive-defaults ()
  (subword-mode +1)
  (run-hooks 'module-lisp-interactive-hook))

(use-package clojure-mode
  :defer t
  :mode "\\.clj\\'"
  :config
  (use-package flycheck-clojure)
  (add-hook 'clojure-mode-hook 'module-clojure-defaults))

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'module-clojure-interactive-defaults))

(provide 'module-clojure)
;;; module-clojure ends here
