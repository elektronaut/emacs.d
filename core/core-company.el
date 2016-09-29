;;; core-company -- Company
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package company
  :bind (("<C-tab>" . company-complete))
  :init
  (setq company-idle-delay 0.5
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
  (global-company-mode 1))

(provide 'core-company)
;;; core-company ends here
