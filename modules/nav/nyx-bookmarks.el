;;; nyx-bookmarks.el --- Bookmarks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Built-in bookmark manager
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1))

(provide 'nyx-bookmarks)
;;; nyx-bookmarks.el ends here
