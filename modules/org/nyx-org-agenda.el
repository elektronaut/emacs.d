;;; nyx-org-agenda-el --- org-mode agenda -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'nyx-org-mode)

(keymap-global-set "C-c a" 'org-agenda)

(defun nyx-skip-agenda-dir (dir)
  "Return non-nil if DIR is not a data dir."
  (and (not (string-match-p "/archive$" dir))
       (not (string-match-p "/data$" dir))))

(setq org-agenda-block-separator 8212
      org-agenda-compact-blocks nil
      org-agenda-files (->> (directory-files-recursively
                             org-directory ".*" t 'nyx-skip-agenda-dir)
                            (-filter #'file-directory-p)
                            (-filter #'nyx-skip-agenda-dir)
                            (append (list org-directory)))
      org-agenda-persistent-filter t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-span 14)

(defvar nyx-stuck-projects-query
  '(and (tags "project")
        (not (done))
        (not (todo "MAYBE" "WAITING"))
        (not (tags "ARCHIVE"))
        (not (descendants (todo "NEXT" "WAITING" "DELEGATED"))))
  "Query for stuck projects.")

(use-package org-ql
  :ensure t
  :config
  (setq org-agenda-custom-commands
        '(("n" "Next actions"
           ((agenda "" ((org-agenda-span 7)))
            (org-ql-block '(and (todo) (priority "A") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "High Priority Tasks")))
            (org-ql-block '(and (todo) (tags "inbox"))
                          ((org-ql-block-header "Inbox")))
            (org-ql-block '(and (todo "NEXT")
                                (not (scheduled))
                                (not (tags "ARCHIVE" "inbox"))
                                (not (ancestors (todo "WAITING" "MAYBE")))
                                (not (priority "A")))
                          ((org-ql-block-header "Next Actions")))
            (org-ql-block nyx-stuck-projects-query
                          ((org-ql-block-header "Stuck Projects")))
            (org-ql-block '(and (todo "WAITING" "DELEGATED") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Waiting/Delegated Items")))))

          ("p" "Projects"
           ((org-ql-block nyx-stuck-projects-query
                          ((org-ql-block-header "Stuck Projects")))
            (org-ql-block '(and (tags "project")
                                (not (done))
                                (not (tags "ARCHIVE"))
                                (not (todo "WAITING" "MAYBE")))
                          ((org-ql-block-header "Active Projects")))
            (org-ql-block '(and (tags "project") (todo "WAITING") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Upcoming projects")))
            (org-ql-block '(and (tags "project") (todo "MAYBE") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Potential projects")))))

          ("w" "Waiting/Delegated"
           ((org-ql-block '(and (todo "WAITING" "DELEGATED") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Waiting/Delegated Items")))
            (org-ql-block '(and (todo "MAYBE") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Maybe")))))

          ("o" "Overview"
           ((agenda "" ((org-agenda-span 14)))
            (org-ql-block nyx-stuck-projects-query
                          ((org-ql-block-header "Stuck Projects")))
            (org-ql-block '(and (todo "NEXT" "TODO")
                                (not (tags "ARCHIVE"))
                                (not (ancestors (todo "WAITING" "MAYBE"))))
                          ((org-ql-block-header "Actionable items")))
            (org-ql-block '(and (todo "WAITING" "DELEGATED") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Waiting/Delegated Items")))
            (org-ql-block '(and (todo "MAYBE") (not (tags "ARCHIVE")))
                          ((org-ql-block-header "Maybe"))))))))

(provide 'nyx-org-agenda)
;;; nyx-org-agenda.el ends here
