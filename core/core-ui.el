;;; core-ui.el --- UI
;;; Commentary:
;;; Code:

;; Enable desktop save mode
(desktop-save-mode 1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Typography
(set-face-attribute 'default        nil :family "Inconsolata" :height 130)
(set-face-attribute 'variable-pitch nil :family "Inconsolata" :height 130)
(setq-default line-spacing 2)

;; Theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")
;; (load-theme 'atom-one-dark t)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(set-mouse-color "white")

;; Cursor
(setq-default cursor-type 'bar)

;; Disable blink on matching paren
(setq blink-matching-paren nil)

;; Highlight the current line
(global-hl-line-mode +1)

;; Frame config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Input config
(setq-default scroll-margin 0
              scroll-conservatively 100000
              scroll-preserve-screen-position 1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable bell when scrolling
(defun core-bell-function ()
  "Custom bell function."
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                  keyboard-quit mwheel-scroll down up next-line previous-line
                  backward-char forward-char minibuffer-keyboard-quit))
    (ding)))
(setq-default ring-bell-function 'core-bell-function)

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


;;-----------------------------------------------------------------------------
;; Packages
;;-----------------------------------------------------------------------------

(use-package ace-window
  :bind (("s-w" . ace-window)))

;; Highlight cursor when changing windows.
(use-package beacon
  :init
  (setq-default beacon-color "#3E4451"
                beacon-blink-duration 0.2
                beacon-blink-delay 0.05)
  :config
  (beacon-mode +1))

;; Delight modes.
(use-package delight
  :config
  (delight '((yas-minor-mode nil yasnippet)
             (abbrev-mode nil abbrev)
             (flyspell-mode nil flyspell)
             (whitespace-mode nil whitespace))))

(use-package diminish)

(use-package discover-my-major
  :bind (("C-c m" . discover-my-major)))

;; Clean up obsolete buffers automatically
(use-package midnight :ensure nil)

(use-package nlinum)

(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Navigate windows with shift+arrow keys
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package winner
  :ensure nil
  :config
  (winner-mode +1))

;; Show the name of the current function definition in the modeline
(use-package which-func
  :ensure nil
  :config
  (which-function-mode 1))

(use-package which-key
  :config
  (which-key-mode +1))

(provide 'core-ui)
;;; core-ui.el ends here
