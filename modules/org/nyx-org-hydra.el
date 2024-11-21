;;; nyx-org-hydra-el --- org-mode hydra -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'nyx-hydra)

(defhydra hydra-org (:hint nil)
  "
  Navigate^^^^                 Status^^^^        Update^^              Insert^^^^                    Go To
  ^^^^^^^^^^^^-------------------------------------------------------------------------------------------------------------
      _↑_/_↓_: heading         ^^_n_: NEXT       _a_: toggle archive   _i t_/_i T_: time (inactive)  _g i_: inbox
    M-_↑_/_↓_: move            ^^_t_: TODO       _A_: move to archive  _i d_/_i D_: date (inactive)  _g a_: anyone
    M-_←_/_→_: shift           ^^_d_: DONE       _r_: refile           ^^^^                          _g p_: personal
  S-M-_←_/_→_: shift subtree   ^^_T_: todo       _S_: schedule         ^^^^                          _g j_: journal
  ^^^^                       _←_/_→_: prev/next  _D_: deadline         ^^^^                          _g A_: areas
  ^^^^                         ^^_,_: priority   _O_: sort             ^^^^                          _g C_: clients

 "
  ;; Navigate
  ("<up>" org-previous-visible-heading)
  ("<down>" org-next-visible-heading)
  ("M-<down>" org-move-subtree-down)
  ("M-<up>" org-move-subtree-up)
  ("M-<left>" org-metaleft)
  ("M-<right>" org-metaright)
  ("S-M-<left>" org-shiftmetaleft)
  ("S-M-<right>" org-shiftmetaright)
  ;; Status
  ("<right>" org-shiftright)
  ("<left>" org-shiftleft)
  ("n" (org-todo "NEXT"))
  ("t" (org-todo "TODO"))
  ("d" (org-todo "DONE"))
  ("T" org-todo)
  ("," org-priority)
  ;; Update
  ("a" org-toggle-archive-tag)
  ("A" org-archive-to-archive-sibling)
  ("r" org-refile)
  ("S" org-schedule)
  ("D" org-deadline)
  ("O" org-sort)
  ;; Insert
  ("i d" org-time-stamp)
  ("i D" org-time-stamp-inactive)
  ("i t" (org-time-stamp t))
  ("i T" (org-time-stamp-inactive t))
  ;; Go to
  ("g i" (find-file org-default-notes-file))
  ("g a" (find-file "~/Library/CloudStorage/Dropbox/org/areas/anyone.org"))
  ("g p" (find-file "~/Library/CloudStorage/Dropbox/org/personal.org"))
  ("g j" (find-file "~/Library/CloudStorage/Dropbox/org/journal.org"))
  ("g O" (dired org-directory))
  ("g A" (dired "~/Library/CloudStorage/Dropbox/org/areas"))
  ("g C" (dired "~/Library/CloudStorage/Dropbox/org/clients"))
  ;; Misc
  ("k" org-cut-subtree "delete")
  ("<tab>" (org-cycle))
  ("q" nil "quit"))

(define-key org-mode-map (kbd "C-c O") #'hydra-org/body)
(define-key org-mode-map (kbd "C-c o") nil)

(provide 'nyx-org-hydra)
;;; nyx-org-hydra.el ends here
