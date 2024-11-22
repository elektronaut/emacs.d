;;; nyx-abbrev.el --- abbrev -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode t
                save-abbrevs t)
  ;;(read-abbrev-file (expand-file-name "abbrev_defs" savefile-dir))
  (add-hook 'text-mode-hook 'abbrev-mode))

(provide 'nyx-abbrev)
;;; nyx-abbrev.el ends here
