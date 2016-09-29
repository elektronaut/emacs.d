;;; module-emacs-lisp -- Emacs lisp
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-flycheck)
(require 'module-lisp)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(defun module-emacs-lisp-recompile-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p emacs-root-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(add-hook 'ielm-mode-hook
          (lambda ()
            (run-hooks 'module-lisp-interactive-hook)
            (eldoc-mode +1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (run-hooks 'module-lisp-hook)
            (eldoc-mode +1)
            (module-emacs-lisp-recompile-on-save)
            (rainbow-mode +1)
            (setq mode-name "EL")))

(provide 'module-emacs-lisp)
;;; module-emacs-lisp ends here
