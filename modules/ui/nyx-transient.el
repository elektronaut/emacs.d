;;; nyx-transient.el --- Transient -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Transient itself is installed eagerly in init.el, before any module loads,
;; so it is never queued as a plain dependency first (which would defeat
;; `:wait').  Here we only need to guarantee it is loaded for modules that call
;; `transient-define-prefix' at top level and `(require 'nyx-transient)'.
(require 'transient)

(provide 'nyx-transient)
;;; nyx-transient.el ends here
