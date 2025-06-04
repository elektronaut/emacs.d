;;; nyx-macos.el --- Settings for MacOS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dired)
(require 'gnutls)

(defvar homebrew-path
  "/opt/homebrew"
  "Homebrew root.")

(when (eq system-type 'darwin)
  (setopt default-input-method "MacOSX"
          system-time-locale "no_NO"
          mac-option-modifier nil
          mac-right-option-modifier nil
          mac-control-modifier 'control
          mac-command-modifier 'meta
          x-select-enable-clipboard t
          mouse-wheel-scroll-amount '(0.01)
          ns-function-modifier 'hyper)

  (add-to-list 'gnutls-trustfiles
               (concat homebrew-path "/etc/openssl/cert.pem"))

  ;; Enable emoji
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; Enable ligatures
  ;;(mac-auto-operator-composition-mode)

  (use-package exec-path-from-shell
    :ensure t
    ;; Use a non-interactive shell, which is much faster
    :custom ((exec-path-from-shell-arguments nil))
    :config
    (exec-path-from-shell-initialize))

  (use-package mise
    :ensure t
    :config
    (global-mise-mode))

  ;; Add Homebrew to the load path
  (let ((default-directory (concat homebrew-path "/share/emacs/site-lisp/")))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Use ls from coreutils. Install coreutils with Homebrew
  ;; to enable.
  (setopt insert-directory-program (concat homebrew-path "/bin/gls")
          dired-use-ls-dired t
          dired-listing-switches "-al --group-directories-first"))

(provide 'nyx-macos)
;;; nyx-macos.el ends here
