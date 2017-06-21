;;; module-org -- Org mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-secrets)
(require 'org)
(require 'org-agenda)

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory "~/Dropbox/org"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-default-notes-file "~/Dropbox/org/organizer.org"
      org-agenda-files '("~/Dropbox/org/organizer.org"
                         "~/Dropbox/org/gcal-anyone.org")
      org-replace-disputed-keys t
      org-src-fontify-natively nil
      org-hide-leading-stars t
      org-log-done t
      org-ellipsis " …")

(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Dropbox/org/gcal-anyone.org" )
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("t" "Task" entry (file+headline "~/Dropbox/org/organizer.org" "Tasks")
	 "* TODO %?\n%u" :prepend t)
	))

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

(use-package org-journal
  :init
  (setq org-journal-dir "~/Dropbox/org/journal"))

(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-dir (expand-file-name "org-gcal/" savefile-dir)
        org-gcal-client-id (config-secret 'gcal-client-id)
	org-gcal-client-secret (config-secret 'gcal-client-secret)
	org-gcal-file-alist '(("inge@anyone.no" .  "~/Dropbox/org/gcal-anyone.org")))
  (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
  :init
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))

(provide 'module-org)
;;; module-org ends here
