;;; core-macos.el --- Settings for macOS
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(when (eq system-type 'darwin)
  (setq-default default-input-method "MacOSX"
                mac-option-modifier nil
                mac-right-option-modifier nil
                mac-control-modifier 'control
                mac-command-modifier 'meta
                x-select-enable-clipboard t
                mouse-wheel-scroll-amount '(0.01)
                ns-function-modifier 'hyper)

  ;; Enable emoji
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; Enable ligatures
  (mac-auto-operator-composition-mode)

  ;; Use ls from coreutils. Install coreutils with Homebrew
  ;; to enable.
  (setq insert-directory-program "/usr/local/bin/gls")

  (use-package exec-path-from-shell
    :defer t
    :commands (exec-path-from-shell-copy-env)
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize)))

(provide 'core-macos)
;;; core-macos.el ends here
