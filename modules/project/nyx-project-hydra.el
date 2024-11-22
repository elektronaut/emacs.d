;;; nyx-project-hydra.el --- Project hydra -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-project)
(require 'nyx-project-org)
(require 'nyx-project-persp)
(require 'nyx-hydra)

(defhydra hydra-project (:hint nil)
  "
  Project: %(project-name (project-current))

  Projects^^               Find^^              Buffer^^               Search^^
  ^^^^^^^^^^------------------------------------------------------------------------
  _p_: Switch perspective  _f_: File           _→_: Next              _s_: rg (quick)
  _P_: Switch              _F_: File (or ext)  _←_: Previous          _S_: rg
  _a_: Add and switch      _d_: Dir            _b_: Switch to buffer
  _x_: Forget              _D_: Dired          _k_: Kill
  _X_: Cleanup             _O_: Org file       _C-s_: Save all

  "
  ("f" project-find-file)
  ("F" project-or-external-find-file)
  ("d" project-find-dir)
  ("D" project-dired)
  ("O" project-org-open)

  ("<right>" project-next-buffer)
  ("<left>" project-previous-buffer)
  ("k" kill-current-buffer)
  ("b" consult-project-buffer)
  ("C-s" project-save-some-buffers)

  ("s" consult-ripgrep)
  ("S" rg-project)

  ("p" project-persp-switch)
  ("P" project-switch-project)
  ("a" project-persp-find-and-switch)
  ("x" project-forget-project)
  ("X" project-forget-zombie-projects)
  ("q" nil "quit"))

(global-set-key (kbd "C-c P") 'hydra-project/body)

(provide 'nyx-project-hydra)
;;; nyx-project-hydra.el ends here
