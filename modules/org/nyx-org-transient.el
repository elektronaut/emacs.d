;;; nyx-org-transient.el --- org-mode transient -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'nyx-transient)

(transient-define-prefix nyx-org-transient ()
  "org-mode transient"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-stay
  [["Navigate"
    ("<up>"        "Previous heading" org-previous-visible-heading)
    ("<down>"      "Next heading" org-next-visible-heading)
    ("M-<up>"      "Move up" org-move-subtree-up)
    ("M-<down>"    "Move down" org-move-subtree-down)
    ("M-<left>"    "Shift left" org-metaleft)
    ("M-<right>"   "Shift right" org-metaright)
    ("S-M-<left>"  "Shift subtree left" org-shiftmetaleft)
    ("S-M-<right>" "Shift subtree right" org-shiftmetaright)
    ("h"           "Go to a heading" consult-org-heading)]
   ["Status"
    ("n" "NEXT" (lambda () (interactive) (org-todo "NEXT")))
    ("t" "TODO" (lambda () (interactive) (org-todo "TODO")))
    ("d" "DONE" (lambda () (interactive) (org-todo "DONE")))
    ("w" "WAITING" (lambda () (interactive) (org-todo "WAITING")))
    ("T" "Select.." org-todo)
    ("<left>" "Previous" org-shiftleft)
    ("<right>" "Next" org-shiftright)
    ("," "Priority" org-priority)]
   ["Update"
    ("a" "Toggle archive" org-toggle-archive-tag)
    ("A" "Move to archive" org-archive-to-archive-sibling)
    ("r" "Refile" org-refile)
    ("S" "Schedule" org-schedule)
    ("D" "Deadline" org-deadline)
    ("O" "Sort" org-sort)]
   ["Insert"
    ("i t" "Time" (lambda () (interactive) (org-time-stamp t)))
    ("i T" "Time (inactive)" (lambda () (interactive) (org-time-stamp-inactive t)))
    ("i d" "Date" org-time-stamp)
    ("i D" "Date (inactive)" org-time-stamp-inactive)
    ("i l" "Link" org-insert-link)]])

(transient-define-prefix nyx-org-roam-transient ()
  "org-roam transient"
  :transient-suffix 'transient--do-quit-one
  [["Node"
    ("f" "Find node" org-roam-node-find)
    ("i" "Insert link to node" org-roam-node-insert)]
   ["Buffer"
    ("l" "Toggle buffer" org-roam-buffer-toggle)]
   ["Dailies"
    ("d" "Today" org-roam-dailies-goto-today)
    ("y" "Yesterday" org-roam-dailies-goto-yesterday)
    ("t" "Tomorrow" org-roam-dailies-goto-tomorrow)
    ("c" "Goto date" org-roam-dailies-goto-date)
    ("n" "Capture today" org-roam-dailies-capture-today)
    ("v" "Capture date" org-roam-dailies-capture-date)
    ("." "Find directory" org-roam-dailies-find-directory)]
   ["Dailies navigation"
    :if org-roam-dailies--daily-note-p
    ("<right>" "Next" org-roam-dailies-goto-next-note :transient 'transient--do-stay)
    ("<left>" "Previous" org-roam-dailies-goto-previous-note :transient 'transient--do-stay)]])

(keymap-set org-mode-map "C-c O" 'nyx-org-transient)
(keymap-unset org-mode-map "C-c o")
(keymap-global-set "C-c C-o" 'nyx-org-roam-transient)

(provide 'nyx-org-transient)
;;; nyx-org-transient.el ends here
