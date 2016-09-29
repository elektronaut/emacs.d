;;; module-smartparens -- Smartparens
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-crux)

(use-package smartparens
  :init
  (setq-default sp-base-key-bindings 'paredit
                sp-autoskip-closing-pair 'always
                sp-hybrid-kill-entire-symbol nil)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  (defun conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (sp-pair "{" nil :post-handlers
	   '(((lambda (&rest _ignored)
		(crux-smart-open-line-above)) "RET")))
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(provide 'module-smartparens)
;;; module-smartparens ends here
