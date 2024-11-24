;;; nyx-abbrev.el --- abbrev -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package abbrev
  :ensure nil
  :hook ((text-mode . abbrev-mode))
  :custom ((save-abbrevs t)))

(provide 'nyx-abbrev)
;;; nyx-abbrev.el ends here
