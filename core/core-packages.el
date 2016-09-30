;;; core-packages.el --- Configuration for packages
;;; Commentary:
;;; Code:

;;-----------------------------------------------------------------------------
;; Built-ins
;;-----------------------------------------------------------------------------

;; Abbreviations
(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode t
                abbrev-file-name (expand-file-name "abbrev_defs" savefile-dir)
                save-abbrevs t)
  (read-abbrev-file (expand-file-name "abbrev_defs" savefile-dir))
  (add-hook 'text-mode-hook 'abbrev-mode))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

;; Calendar
(use-package calendar
  :ensure nil
  :init
  (setq-default calendar-week-start-day 1))

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)
  (use-package dired-x :ensure nil))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eshell
  :ensure nil
  :bind (("C-x m" . eshell))
  :init
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir))
  :config
  ;; Start a new eshell even if one is active.
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))))

(use-package hippie-expand
  :ensure nil
  :bind (("s-/" . hippie-expand)
         ("M-/" . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package mu4e
  :ensure nil
  :init
  (setq-default mu4e-maildir          (expand-file-name "~/Mail/elektronaut")
                mu4e-drafts-folder    "/Drafts"
                mu4e-sent-folder      "/Sent Messages"
                mu4e-trash-folder     "/Trash"
                mu4e-get-mail-command "offlineimap"
                ;;mu4e-sent-messages-behavior 'delete
                mu4e-maildir-shortcuts '(("/"              . ?i)
                                         ("/Sent Messages" . ?s)
                                         ("/Trash"         . ?t))))

;; Clean up obsolete buffers automatically
(use-package midnight :ensure nil)

(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (setq-default save-place t))

;; Navigate windows with shift+arrow keys
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

;; Show the name of the current function definition in the modeline
(use-package which-func
  :ensure nil
  :config
  (which-function-mode 1))

(use-package winner
  :ensure nil
  :config
  (winner-mode +1))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh"))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


;;-----------------------------------------------------------------------------
;; Extra packages
;;-----------------------------------------------------------------------------

(use-package ace-window
  :bind (("s-w" . ace-window)))

(use-package anzu
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package avy
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-word-or-subword-1))
  :init
  (setq avy-background t
        avy-style 'at-full))

(use-package beacon
  :init
  (setq-default beacon-color "#3E4451"
                beacon-blink-duration 0.2
                beacon-blink-delay 0.05)
  :config
  (beacon-mode +1))

(use-package browse-kill-ring
  :bind (("s-y" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

(use-package calendar-norway
  :config
  (setq-default calendar-holidays
                (append
                 calendar-norway-raude-dagar
                 calendar-norway-andre-merkedagar
                 calendar-norway-dst)))

(use-package delight
  :config
  (delight '((yas-minor-mode nil yasnippet)
             (abbrev-mode nil abbrev)
             (flyspell-mode nil flyspell)
             (whitespace-mode nil whitespace))))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package diminish)

(use-package discover-my-major
  :bind (("C-c m" . discover-my-major)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(use-package god-mode
  :bind (("s-g" . god-local-mode)))

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)))

(use-package move-text
  :config
  (global-set-key [(control shift up)]  'move-text-up)
  (global-set-key [(control shift down)]  'move-text-down)
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down))

(use-package multiple-cursors
  :bind (("C-S-e"       . mc/edit-lines)
         ("C-S-n"       . mc/mark-next-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" savefile-dir)))

(use-package project-explorer
  :bind (("C-c p x" . project-explorer-open)))

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package smartrep)
(use-package operate-on-number
  :config
  (smartrep-define-key global-map "C-c ."
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("'" . operate-on-number-at-point))))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package viking-mode
  :config
  (viking-global-mode))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package wgrep-ag
  :bind (("C-c p s S" . projectile-ag)))

(use-package yari
  :init
  (define-key 'help-command (kbd "R") 'yari))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(provide 'core-packages)
;;; core-packages.el ends here
