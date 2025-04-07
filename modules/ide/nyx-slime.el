;;; nyx-slime.el --- SLIME -*- lexical-binding: t; -*-

;;; Commentary:
;;;   SLIME is the Superior Lisp Interaction Mode for Emacs.
;;;
;;;   SLIME extends Emacs with support for interactive programming in
;;;   Common Lisp.  The features are centered around slime-mode, an
;;;   Emacs minor-mode that complements the standard lisp-mode.  While
;;;   lisp-mode supports editing Lisp source files, slime-mode adds
;;;   support for interacting with a running Common Lisp process for
;;;   compilation, debugging, documentation lookup, and so on.

;;; Code:
(use-package slime
  :ensure t
  :bind (("C-c C-s" . slime-selector))
  :init
  ;; A list of alternative Common Lisp implementations that can be
  ;; used with SLIME. Note that their presence render
  ;; inferior-lisp-program useless. This variable holds a list of
  ;; programs and if you invoke SLIME with a negative prefix
  ;; argument, M-- M-x slime, you can select a program from that list.
  (setopt slime-lisp-implementations
          '((ccl ("ccl"))
            (clisp ("clisp" "-q"))
            (cmucl ("cmucl" "-quiet"))
            (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  (if (and (eq system-type 'darwin)
           (executable-find "ccl"))
      ;; default to Clozure CL on OS X
      (setopt slime-default-lisp 'ccl)
    ;; default to SBCL on Linux and Windows
    (setopt slime-default-lisp 'sbcl))
  :config
  (setopt slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          slime-fuzzy-completion-in-place t
          slime-enable-evaluate-in-emacs t
          slime-auto-start 'always
          slime-contribs '(slime-fancy))
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (smartparens-strict-mode +1)
              (whitespace-mode -1))))

(provide 'nyx-slime)
;;; nyx-slime.el ends here
