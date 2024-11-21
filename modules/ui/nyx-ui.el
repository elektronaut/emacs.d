;;; nyx-ui.el --- UI config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Cursor
(setq-default cursor-type 'bar)

;; Disable blink on matching paren
(setq blink-matching-paren nil)

;; Highlight the current line
(global-hl-line-mode +1)

;; Frame config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode 1)
(scroll-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Scrolling
(setq-default scroll-margin 0
              scroll-conservatively 100000
              scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; Disable bell when scrolling
(defun nyx-bell-function ()
  "Custom bell function."
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char minibuffer-keyboard-quit))
    (ding)))
(setq-default ring-bell-function 'nyx-bell-function
              visible-bell 'top-bottom)

;; Frame title
(setq-default frame-title-format
              '(""
                (:eval
                 (if (and (bound-and-true-p projectile-mode)
                          (projectile-project-p))
                     (format "%s" (projectile-project-name))
                   (if (buffer-file-name)
                       (abbreviate-file-name (buffer-file-name))
                     "%b")
                   ))))

(provide 'nyx-ui)
;;; nyx-ui.el ends here
