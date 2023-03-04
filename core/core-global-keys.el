;;; core-global-keys.el --- Global key bindings
;;; Commentary:
;;; Code:

(require 'core-defuns)

(use-package god-mode
  :bind (("s-g" . god-local-mode)))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "C-M-,") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-+") 'auto-window-layout)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-x 3") 'split-window-and-balance)
(global-set-key (kbd "C-x 0") 'delete-window-and-balance)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
    (global-set-key (kbd "C-x p") 'proced))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; More mac-like movement keys
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-<right>") 'move-end-of-line)
(global-set-key (kbd "A-<left>") 'left-word)
(global-set-key (kbd "A-<right>") 'right-word)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(provide 'core-global-keys)
;;; core-global-keys.el ends here
