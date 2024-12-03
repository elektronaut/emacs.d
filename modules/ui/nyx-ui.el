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
                                backward-char forward-char minibuffer-keyboard-quit
                                nyx-keyboard-quit))
    (ding)))
(setq-default ring-bell-function 'nyx-bell-function
              visible-bell 'top-bottom)

;; Frame title
(setq-default frame-title-format
              '(""
                (:eval
                 (if (project-current)
                     (format "%s" (project-name (project-current)))
                   (if (buffer-file-name)
                       (abbreviate-file-name (buffer-file-name))
                     "%b")
                   ))))

;; Taken from
;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun nyx-keyboard-quit ()
  "Enhanced `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(keymap-global-set "C-g" #'nyx-keyboard-quit)

(provide 'nyx-ui)
;;; nyx-ui.el ends here
