;;; nyx-project-transient.el --- project-mode transient -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-project)
(require 'nyx-project-org)
(require 'nyx-project-persp)
(require 'nyx-transient)

(transient-define-prefix nyx-project-transient ()
  "project.el transient"
  :transient-suffix 'transient--do-stay
  [["Projects"
    ("p" "Switch" project-persp-switch)
    ("P" "Switch (in this perspective)" project-switch-project)
    ("a" "Add and switch" project-persp-find-and-switch)
    ("x" "Forget" project-forget-project)
    ("X" "Forget zombie projects" project-forget-zombie-projects)]
   ["Find"
    ("f" "File" project-find-file)
    ("F" "File (or external)" project-or-external-find-file)
    ("d" "Dir" project-find-dir)
    ("D" "Dired" project-dired :transient transient--do-quit-one)
    ("O" "Open Org file" project-org-open)]
   ["Buffer"
    ("<right>" "Next" project-next-buffer)
    ("<left>" "Previous" project-previous-buffer)
    ("b" "Switch to buffer" consult-project-buffer)
    ("k" "Kill" kill-current-buffer)
    ("C-s" "Save all" project-save-some-buffers)]
   ["Search"
    ("s" "rg (minibuffer)" consult-ripgrep)
    ("S" "rg (wgrep)" rg-project :transient transient--do-quit-one)]])

(global-set-key (kbd "C-c P") 'nyx-project-transient)

(provide 'nyx-project-transient)
;;; nyx-project-transient.el ends here
