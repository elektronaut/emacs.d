;;; nyx-persp-transient.el --- persp-mode transient -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-persp)
(require 'nyx-transient)

(transient-define-prefix nyx-persp-transient ()
  "persp-mode transient"
  :transient-suffix 'transient--do-stay
  [["Navigate"
    ("<left>" "Previous" persp-prev)
    ("<right>" "Next" persp-next)
    ("h" "Hide" persp-hide)
    ("u" "Unhide" persp-unhide)]
   ["Manage"
    ("c" "Copy" persp-copy)
    ("r" "Rename" persp-rename)
    ("C" "Kill" persp-kill-current)
    ("O" "Kill all others" persp-kill-other)]
   ["Buffers"
    ("a" "Add" persp-add-buffer)
    ("k" "Remove" persp-remove-buffer)
    ("K" "Kill" persp-kill-buffer)
    ("b" "Switch" persp-switch-to-buffer)
    ("t" "Temporarily display" persp-temporarily-display-buffer)
    ("B" "ibuffer" persp-ibuffer :transient transient--do-quit-one)]])

(define-key persp-mode-map (kbd "C-x X") #'nyx-persp-transient)

(provide 'nyx-persp-transient)
;;; nyx-persp-transient.el ends here
