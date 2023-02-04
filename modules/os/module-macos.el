;;; module-macos.el --- Settings for MacOS
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)
  (setq-default default-input-method "MacOSX"
                system-time-locale "no_NO"
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
  ;;(mac-auto-operator-composition-mode)

  ;; Use ls from coreutils. Install coreutils with Homebrew
  ;; to enable.
  (setq insert-directory-program
        (concat homebrew-path "/bin/gls")))

(provide 'module-macos)
;;; module-macos.el ends here
