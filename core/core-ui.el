;;; core-ui.el --- UI
;;; Commentary:
;;; Code:

(require 'core-hydra)

(defvar savefile-dir)

;; Disable desktop save mode
(desktop-save-mode 0)

;; Open org agenda by default
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (persp-switch "org")
;;             (switch-to-buffer (find-file "~/Library/CloudStorage/Dropbox/org/anyone.org"))))

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

;; Window split direction
(setq split-width-threshold 130)
(setq split-height-threshold nil)

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
(setq-default ring-bell-function 'core-bell-function
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


;;-----------------------------------------------------------------------------
;; Packages
;;-----------------------------------------------------------------------------

(use-package ace-window
  :ensure t
  :bind (("s-w" . ace-window)))

;; Highlight cursor when changing windows.
;; (use-package beacon
;;   :init
;;   (setq-default beacon-color "#3E4451"
;;                 beacon-blink-duration 0.2
;;                 beacon-blink-delay 0.05)
;;   :config
;;   (beacon-mode +1))

;; Delight modes.
(use-package delight
  :ensure t
  :config
  (delight '((yas-minor-mode nil yasnippet)
             (abbrev-mode nil abbrev)
             (flyspell-mode nil flyspell)
             (whitespace-mode nil whitespace))))

(use-package diminish
  :ensure t)

(use-package discover-my-major
  :ensure t
  :bind (("C-c m" . discover-my-major)))

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

(use-package smex
  :ensure t
  :init
  (setq smex-save-file (expand-file-name "smex-items" savefile-dir))
  :config
  (smex-initialize))

(require 'thingatpt)

;; Navigate windows with shift+arrow keys
(use-package windmove
  :defer t
  :ensure nil
  :init
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
  :ensure t
  :config
  (which-key-mode +1))


(defun swap-window-direction (dir)
  "Move selected window in DIR."
  (let ((target-window (window-in-direction dir)))
    (if target-window
        (window-swap-states (selected-window) target-window)
      (message "Cannot move window"))))

(defhydra hydra-window (:hint nil)
  "
  Window

  Navigate^^^^
  ^^^^^^^^----------------------------------------------------------
    _↑_/_↓_/_←_/_→_: navigate
  S-_↑_/_↓_/_←_/_→_: move window
  ^^^^^^        _2_: split vertical
  ^^^^^^        _3_: split horizontal
  ^^^^^^        _x_: delete

  "
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<S-left>" (swap-window-direction 'left))
  ("<S-right>" (swap-window-direction 'right))
  ("<S-up>" (swap-window-direction 'up))
  ("<S-down>" (swap-window-direction 'down))
  ("2" split-window-below)
  ("3" split-window-and-balance)
  ("x" delete-window-and-balance)
  ("+" balance-windows)
  ("a" ace-window)
  ("s" ace-swap-window)
  ("q" nil "quit"))

(define-key global-map (kbd "C-x w") #'hydra-window/body)

(provide 'core-ui)
;;; core-ui.el ends here
