;;; module-org -- Org mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-replace-disputed-keys t
      org-src-fontify-natively t
      org-hide-leading-stars nil
      org-log-done t)

(use-package org-journal
  :init
  (setq org-journal-dir "~/Dropbox/org/journal"))

(provide 'module-org)
;;; module-org ends here
