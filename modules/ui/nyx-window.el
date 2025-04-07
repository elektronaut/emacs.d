;;; nyx-window.el --- Window config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Window split direction
(setopt split-width-threshold 130
        split-height-threshold nil
        switch-to-buffer-obey-display-actions t)

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

;; Avoid resizing
(customize-set-variable 'even-window-sizes nil)

(keymap-global-set "C-x 3" 'split-window-and-balance)
(keymap-global-set "C-x 0" 'delete-window-and-balance)
(keymap-global-set "C-x C-+" 'auto-window-layout)

;; Window switching. (C-x o goes to the next window)
(keymap-global-set "C-x O" (lambda () (interactive) (other-window -1)))

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
  :custom ((aw-dispatch-always t))
  :bind (("C-S-o" . ace-window)))

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

(transient-define-prefix nyx-window-transient ()
  "window transient"
  :transient-suffix 'transient--do-stay
  [["Navigate"
    ("<left>" "Left" windmove-left)
    ("<right>" "Right" windmove-right)
    ("<up>" "Up" windmove-up)
    ("<down>" "Down" windmove-down)]
   ["Move"
    ("<S-left>" "Left" (lambda () (interactive) (swap-window-direction 'left)))
    ("<S-right>" "Right" (lambda () (interactive) (swap-window-direction 'right)))
    ("<S-up>" "Up" (lambda () (interactive) (swap-window-direction 'up)))
    ("<S-down>" "Down" (lambda () (interactive) (swap-window-direction 'down)))]]
  ["Split"
   ("2" "Split vertical" split-window-below)
   ("3" "Split horizontal" split-window-and-balance)
   ("x" "Delete" delete-window-and-balance)
   ("+" "Balance" balance-windows)]
  ["Utils"
   ("a" "ace-window" ace-window)
   ("s" "ace-swap-window" ace-swap-window)])

(keymap-set global-map "C-x w" 'nyx-window-transient)

(provide 'nyx-window)
;;; nyx-window.el ends here
