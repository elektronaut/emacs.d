;;; nyx-company.el --- Company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :bind (("<C-tab>" . company-complete))
  :custom ((company-idle-delay 3)
           (company-show-quick-access t)
           (company-tooltip-limit 10)
           (company-minimum-prefix-length 2)
           (company-tooltip-flip-when-above t))
  :config
  (global-company-mode 1))

(use-package company-emoji
  :ensure t
  :after (company)
  :init
  (add-to-list 'company-backends 'company-emoji))

(provide 'nyx-company)
;;; nyx-company.el ends here
