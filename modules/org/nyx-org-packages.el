;;; nyx-org-packages-el --- org-mode packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; org-autolist makes org-mode lists behave more like lists in
;; non-programming editors such as Google Docs, MS Word, and
;; OS X Notes.
(use-package org-autolist
  :ensure t
  :hook ((org-mode . org-autolist-mode)))

;; This module copies selected regions in org-mode as formatted text
;; on the clipboard that can be pasted into other applications.
(use-package ox-clip
  :ensure t
  :commands (ox-clip-formatted-copy
             ox-clip-image-to-clipboard))

;; org-mime can be used to send HTML email using Org-mode HTML export.
(use-package org-mime
  :ensure t
  :commands (org-mime-htmlize
             org-mime-org-buffer-htmlize
             org-mime-org-subtree-htmlize)
  :custom (org-mime-export-options
           '(:section-numbers nil :with-author nil :with-toc nil)))

(provide 'nyx-org-packages)
;;; nyx-org-packages.el ends here
