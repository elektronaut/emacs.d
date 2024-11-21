;;; nyx-multiple-cursors.el --- Multiple cursors -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-savefiles)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-e"       . mc/edit-lines)
         ("C-S-n"       . mc/mark-next-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :custom
  (mc/list-file (expand-file-name "mc-lists.el" savefile-dir)))

(provide 'nyx-multiple-cursors)
;;; nyx-multiple-cursors.el ends here
