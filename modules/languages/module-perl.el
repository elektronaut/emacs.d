;;; module-perl -- Perl
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(defalias 'perl-mode 'cperl-mode)

(define-key 'help-command "P" 'cperl-perldoc)

(defun module-perl-defaults ()
  (setq cperl-indent-level 4
        cperl-continued-statement-offset 8
        cperl-font-lock t
        cperl-electric-lbrace-space t
        cperl-electric-parens nil
        cperl-electric-linefeed nil
        cperl-electric-keywords nil
        cperl-info-on-command-no-prompt t
        cperl-clobber-lisp-bindings t
        cperl-lazy-help-time 3
        cperl-invalid-face nil)
  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil))

(add-hook 'cperl-mode-hook 'module-perl-defaults)

(provide 'module-perl)
;;; module-perl ends here
