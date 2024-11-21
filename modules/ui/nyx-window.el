;;; nyx-window.el --- Window config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-hydra)

;; Window split direction
(setq split-width-threshold 130
      split-height-threshold nil)

(global-set-key (kbd "C-x 3") 'split-window-and-balance)
(global-set-key (kbd "C-x 0") 'delete-window-and-balance)
(global-set-key (kbd "C-x C-+") 'auto-window-layout)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun swap-window-direction (dir)
  "Move selected window in DIR."
  (let ((target-window (window-in-direction dir)))
    (if target-window
        (window-swap-states (selected-window) target-window)
      (message "Cannot move window"))))

(defun split-n (n)
  "Split frame in N windows."
  (let ((split-count (- n (count-windows))))
    (if (> split-count 0) (dotimes (i split-count) (split-window-right)))
    (if (< split-count 0) (dotimes (i (abs split-count))
                            (other-window -1)
                            (delete-window)
                            (other-window 1)))
    (unless (eq split-count 0) (balance-windows))))

(defun split-2 () "Split frame in 2 windows." (interactive) (split-n 2))
(defun split-3 () "Split frame in 3 windows." (interactive) (split-n 3))
(defun split-4 () "Split frame in 4 windows." (interactive) (split-n 4))

(defun split-window-and-balance ()
  "Split window and balance."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun delete-window-and-balance ()
  "Delete window and balance."
  (interactive)
  (delete-window)
  (balance-windows))

(defun auto-window-layout ()
  "Automatically layout frame in 2 or 3 windows depending on size."
  (interactive)
  (if (> (frame-width) (* 3 84))
      (split-n 3)
    (split-n 2))
  (balance-windows))

(use-package ace-window
  :ensure t
  :bind (("s-w" . ace-window)))

;; Navigate windows with shift+arrow keys
(use-package windmove
  :defer t
  :ensure nil
  :init
  (windmove-default-keybindings))

;; Undo window configuration change
(use-package winner
  :ensure nil
  :config
  (winner-mode +1))

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

(provide 'nyx-window)
;;; nyx-window.el ends here
