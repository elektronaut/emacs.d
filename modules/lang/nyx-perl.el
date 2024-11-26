;;; nyx-perl.el --- Perl -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (defalias 'perl-mode 'cperl-mode)

(keymap-global-set "C-h P" 'cperl-perldoc)

(use-package cperl-mode
  :ensure nil
  :mode "\\.pl\\'"
  :custom ((cperl-indent-level 4)
           (cperl-continued-statement-offset 8)
           (cperl-font-lock t)
           (cperl-electric-lbrace-space t)
           (cperl-electric-parens nil)
           (cperl-electric-linefeed nil)
           (cperl-electric-keywords nil)
           (cperl-info-on-command-no-prompt t)
           (cperl-clobber-lisp-bindings t)
           (cperl-lazy-help-time 3)
           (cperl-invalid-face nil)))

(provide 'nyx-perl)
;;; nyx-perl.el ends here
