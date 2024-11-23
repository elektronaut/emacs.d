;;; nyx-org-transient.el --- org-mode transient -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'nyx-transient)

(transient-define-prefix nyx-org-transient ()
  "org-mode transient"
  :transient-suffix 'transient--do-stay
  [["Navigate"
    ("<up>"        "Previous heading" org-previous-visible-heading)
    ("<down>"      "Next heading" org-next-visible-heading)
    ("M-<up>"      "Move up" org-move-subtree-up)
    ("M-<down>"    "Move down" org-move-subtree-down)
    ("M-<left>"    "Shift left" org-metaleft)
    ("M-<right>"   "Shift right" org-metaright)
    ("S-M-<left>"  "Shift subtree left" org-shiftmetaleft)
    ("S-M-<right>" "Shift subtree right" org-shiftmetaright)]
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
    ("i t" "Date" org-time-stamp)
    ("i T" "Date (inactive)" org-time-stamp-inactive)]
   ["Go to"
    ("g i" "Inbox" (lambda () (interactive) (find-file org-default-notes-file)))
    ("g a" "Anyone" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/org/areas/anyone.org")))
    ("g p" "Personal" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/org/personal.org")))
    ("g j" "Journal" (lambda () (interactive) (find-file "~/Library/CloudStorage/Dropbox/org/journal.org")))
    ("g d" "Org directory" (lambda () (interactive) (dired org-directory)) :transient transient--do-quit-one)
    ("g A" "Areas" (lambda () (interactive) (dired "~/Library/CloudStorage/Dropbox/org/areas"))
     :transient transient--do-quit-one)
    ("g C" "Clients" (lambda () (interactive) (dired "~/Library/CloudStorage/Dropbox/org/clients"))
     :transient transient--do-quit-one)]])

(define-key org-mode-map (kbd "C-c O") #'nyx-org-transient)
(define-key org-mode-map (kbd "C-c o") nil)

(provide 'nyx-org-transient)
;;; nyx-org-transient.el ends here
