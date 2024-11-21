;;; nyx-projectile-hydra.el --- Projectile hydra -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-projectile)
(require 'nyx-projectile-org)
(require 'nyx-projectile-persp)
(require 'nyx-hydra)

(defhydra hydra-projectile (:hint nil)
  "
  Projectile: %(projectile-project-name)

  Find^^             Buffer^^                Search^^          Projects^^
  ^^^^^^^^^^------------------------------------------------------------------------
  _f_: file          _→_: next               _s_: rg (quick)   _p_: switch perspective
  _r_: recent file   _←_: previous           _S_: rg           _P_: switch
  _F_: file in dir   _k_: kill               ^^                _A_: add
  _d_: dir           _b_: switch to buffer   ^^                _x_: remove
  _D_: root dir      _B_: ibuffer            ^^                _X_: cleanup
  _O_: org file    _C-s_: save all           ^^

  "
  ("f" consult-projectile-find-file)
  ("r" consult-projectile-recentf)
  ("F" projectile-find-file-in-directory)
  ("d" consult-projectile-find-dir)
  ("D" projectile-dired)
  ("O" projectile-open-org)

  ("<right>" projectile-next-project-buffer)
  ("<left>" projectile-previous-project-buffer)
  ("k" kill-current-buffer)
  ("b" consult-project-buffer)
  ("B" projectile-ibuffer)
  ("C-s" projectile-save-project-buffers)

  ("s" consult-ripgrep)
  ("S" rg-project)

  ("p" projectile-switch-persp-project)
  ("P" projectile-switch-project)
  ("A" projectile-add-known-project)
  ("x" projectile-remove-known-project)
  ("X" projectile-cleanup-known-projects)
  ("q" nil "quit"))

(define-key projectile-mode-map (kbd "C-c P") #'hydra-projectile/body)

(provide 'nyx-projectile-hydra)
;;; nyx-projectile-hydra.el ends here
