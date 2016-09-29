;;; core-macos.el --- Settings for macOS
;;; Commentary:
;;; Code:

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

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package vkill
  :bind (("C-x p" . vkill)))

(provide 'core-macos)
;;; core-macos.el ends here
