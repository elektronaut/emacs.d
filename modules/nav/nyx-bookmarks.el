;;; nyx-bookmarks.el --- Bookmarks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-savefiles)

;; Built-in bookmark manager
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

(provide 'nyx-bookmarks)
;;; nyx-bookmarks.el ends here
