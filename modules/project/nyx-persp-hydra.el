;;; nyx-persp-hydra.el --- persp-mode hydra -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-hydra)
(require 'nyx-persp)

(defhydra hydra-persp (:hint nil)
  "
  Perspective: %(safe-persp-name (get-current-persp))

  Navigate^^^^        Manage^^              Buffers
  ^^^^^^^^----------------------------------------------------------
  _←_/_→_: prev/next  _c_: copy             _a_: add
  ^^  _s_: switch     _r_: rename           _k_: remove
  ^^  _h_: hide       _C_: kill             _K_: kill
  ^^  _u_: unhide     _O_: kill all others  _b_: switch
  ^^^^                ^^                    _t_: temporarily display
  ^^^^                ^^                    _B_: ibuffer

  "
  ("<left>" persp-prev)
  ("<right>" persp-next)
  ("s" persp-switch)
  ("h" persp-hide)
  ("u" persp-unhide)
  ("c" persp-copy)
  ("r" persp-rename)
  ("C" persp-kill-current)
  ("O" persp-kill-other)
  ("a" persp-add-buffer)
  ("k" persp-remove-buffer)
  ("K" persp-kill-buffer)
  ("b" persp-switch-to-buffer)
  ("t" persp-temporarily-display-buffer)
  ("B" persp-ibuffer :color blue)
  ("q" nil "quit"))

(define-key persp-mode-map (kbd "C-x X") #'hydra-persp/body)

(provide 'nyx-persp-hydra)
;;; nyx-persp-hydra.el ends here
