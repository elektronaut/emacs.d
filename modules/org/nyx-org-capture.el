;;; nyx-org-capture-el --- org-mode capture -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-capture)
(require 'nyx-projectile)
(require 'nyx-projectile-org)
(require 'nyx-org-mode)

(global-set-key "\C-cc" 'nyx-org-capture)

(defvar nyx-org-capture-default-target
  (concat org-directory "/inbox.org"))

(defvar nyx-org-capture-inbox-target
  nyx-org-capture-default-target
  "Filename for `org-capture'.")

(defvar nyx-org-capture-email-target
  nyx-org-capture-default-target
  "Filename for `org-capture'.")

(setq org-capture-templates
      '(("t" "Task" entry (file+olp nyx-org-capture-inbox-target "Tasks")
         "* TODO %?\n%U\n%i" :prepend t)
        ("T" "Task (with link)" entry (file+olp nyx-org-capture-inbox-target "Tasks")
         "* TODO %?\n%U\n%a\n%i" :prepend t)
        ("n" "Note" entry (file+olp nyx-org-capture-inbox-target "Notes")
         "* %? %U\n%i" :prepend t)
        ("s" "Source note" entry (file+olp nyx-org-capture-inbox-target "Notes")
         "* %? %U\n#+BEGIN_SRC\n%i\n#+END_SRC\nFrom: %a" :prepend t)
        ("j" "Journal entry" entry (file+datetree "~/Library/CloudStorage/Dropbox/org/journal.org")
         "* %?\n%i")
        ("m" "Email workflow")
        ("mf" "Follow up" entry (file+olp nyx-org-capture-email-target "Email")
         "* NEXT Follow up with %:fromname on %a\n\n%i"
         :prepend t :immediate-finish t)
        ("mr" "Read later" entry (file+olp nyx-org-capture-email-target "Email")
         "* NEXT Read %a\n\n%i" :prepend t :immediate-finish t)))

(defun nyx-org-capture ()
  "Set `nyx-org-capture-inbox-target' and call `org-capture'."
  (interactive)
  (setq nyx-org-capture-inbox-target
        (if (and (projectile-project-p)
                 (file-exists-p (projectile-org-file)))
            (projectile-org-file)
          nyx-org-capture-default-target))
  (call-interactively #'org-capture))

(provide 'nyx-org-capture)
;;; nyx-org-capture.el ends here
