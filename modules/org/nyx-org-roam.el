;;; nyx-org-roam-el --- org-roam config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Library/CloudStorage/Dropbox/org-roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o i" . org-roam-node-insert))
  :bind-keymap ("C-c o d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-enable))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

(provide 'nyx-org-roam)
;;; nyx-org-roam.el ends here
