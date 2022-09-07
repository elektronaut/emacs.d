;;; core-expand -- Expand completion
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode t
                abbrev-file-name (expand-file-name "abbrev_defs" savefile-dir)
                save-abbrevs t)
  (read-abbrev-file (expand-file-name "abbrev_defs" savefile-dir))
  (add-hook 'text-mode-hook 'abbrev-mode))

(use-package company
  :bind (("<C-tab>" . company-complete))
  :init
  (setq company-idle-delay 0.5
        company-show-quick-access t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  :config
  (use-package company-emoji
    :init
    (add-to-list 'company-backends 'company-emoji))
  (use-package company-web-html
    :ensure company-web
    :init
    (add-to-list 'company-backends 'company-web-html))
  (company-tng-mode)
  (global-company-mode 1))

(use-package hippie-expand
  :ensure nil
  :bind (("s-/" . hippie-expand)
         ("M-/" . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package lorem-ipsum)

(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))

(provide 'core-expand)
;;; core-expand.el ends here
