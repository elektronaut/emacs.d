;;; module-org -- Org mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-secrets)
(require 'org)
(require 'org-agenda)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org-archive\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory "~/Dropbox/org"
      org-agenda-compact-blocks nil
      org-agenda-files '("~/Dropbox/org")
      org-agenda-persistent-filter t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-span 14
      org-archive-location "%s-archive::datetree/"
      org-cycle-separator-lines 2
      org-default-notes-file "~/Dropbox/org/organizer.org"
      org-ellipsis nil
      org-hide-leading-stars t
      org-log-done t
      org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-replace-disputed-keys t
      org-reverse-note-order t
      org-src-fontify-natively t
      org-startup-indented t)

(setq org-agenda-custom-commands
      '(("n" "Next actions"
         ((agenda "" ((org-agenda-span 7)))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High priority")))
          (todo "NEXT"  ((org-agenda-overriding-header "Next actions")))
          (todo "WAITING"  ((org-agenda-overriding-header "Waiting")))
          (stuck ""  ((org-agenda-overriding-header "Stuck projects")))))
        ("o" "Overview"
         ((agenda "" ((org-agenda-span 14)))
          (stuck "" ((org-agenda-overriding-header "Stuck projects")))
          (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
          (todo "TODO" ((org-agenda-overriding-header "Actionable tasks")))
          (todo "MAYBE" ((org-agenda-overriding-header "Someday/maybe")))
          (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))))))

(setq org-stuck-projects
      '("+project+LEVEL=2/-DONE-MAYBE-CANCELLED" ("NEXT" "TODO" "WAITING") nil ""))

(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING" "MAYBE" "|"
                  "DONE(d)" "DELEGATED" "CANCELLED")))

(setq org-todo-keyword-faces
      (quote (("NEXT"    :foreground "#98be65" :weight bold)
              ("TODO"    :foreground "#98be65" :weight bold)
              ("WAITING" :foreground "#51afef" :weight bold)
              ("MAYBE"   :foreground "#51afef" :weight bold))))

(setq org-capture-templates
      '(("t" "Task" entry (file+olp "~/Dropbox/org/organizer.org" "Tasks")
	 "* TODO %?\n%U\n%i" :prepend t)
        ("n" "Note" entry (file+olp "~/Dropbox/org/organizer.org" "Notes")
	 "* %? %U\n%i" :prepend t)
        ("s" "Source note" entry (file+olp "~/Dropbox/org/organizer.org" "Notes")
	 "* %? %U\n#+BEGIN_SRC\n%i\n#+END_SRC\nFrom: %a" :prepend t)
        ("j" "Journal entry" entry (file+datetree "~/Dropbox/org/journal.org")
	 "* %?\n%i")))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand"  . ?e)
                            ("@office"  . ?o)
                            ("@home"    . ?h)
                            (:endgroup)
                            ("project"  . ?p)
                            ("personal" . ?P)
                            ("band"     . ?b)
                            ("pedals")
                            ("work"     . ?w)
                            ("note"     . ?n)
                            ("flagged"  . ??))))

(with-eval-after-load 'org
  ;; Prevent Org from overriding the bindings for windmove.
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<up>") nil)
  (define-key org-agenda-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<left>") nil)
  (define-key org-agenda-mode-map (kbd "S-<right>") nil)

  ;; Add replacements for the some of keybindings we just removed. It
  ;; looks like Org already binds C-up and C-down separately from M-{
  ;; and M-}, so we can't use those. Users will just have to make do
  ;; with C-c <up> and C-c <down> for now.
  ;;
  ;; Now for Org Agenda on the other hand, we could use C-up and
  ;; C-down because M-{ and M-} are bound to the same commands. But I
  ;; think it's best to take the same approach as before, for
  ;; consistency.
  (define-key org-mode-map (kbd "C-<left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "C-<right>") #'org-shiftright)
  (define-key org-agenda-mode-map (kbd "C-<left>") #'org-agenda-do-date-earlier)
  (define-key org-agenda-mode-map (kbd "C-<right>") #'org-agenda-do-date-later))

(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

;; (use-package org-journal
;;   :init
;;   (setq org-journal-dir "~/Dropbox/org/journal"))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-dir (expand-file-name "org-gcal/" savefile-dir)
;;         org-gcal-client-id (config-secret 'gcal-client-id)
;; 	org-gcal-client-secret (config-secret 'gcal-client-secret)
;; 	org-gcal-file-alist '(("inge@anyone.no" .  "~/Dropbox/org/gcal-anyone.org")))
;;   (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
;;   :init
;;   (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;   (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))

(provide 'module-org)
;;; module-org ends here
