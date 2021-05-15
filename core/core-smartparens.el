;;; core-smartparens -- Smartparens
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-crux)

(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ;("M-<up>" . sp-up-sexp)
              ;("M-<down>" . sp-down-sexp)
              ("C-M-<up>" . sp-backward-up-sexp)
              ("C-M-<down>" . sp-backward-down-sexp)
              ("C-M-<right>" . sp-forward-sexp)
              ("C-M-<left>" . sp-backward-sexp)

              ("C-M-S-<up>" . sp-unwrap-sexp)
              ("C-M-S-<down>" . sp-wrap-round)

              ("C-S-<left>" . sp-forward-barf-sexp)
              ("C-S-<right>" . sp-forward-slurp-sexp)
              ("C-M-S-<left>" . sp-backward-slurp-sexp)
              ("C-M-S-<right>" . sp-backward-barf-sexp))
  :init
  (setq-default ;; sp-base-key-bindings 'paredit
   sp-autoskip-closing-pair 'always
   sp-hybrid-kill-entire-symbol nil)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  (defun conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
  ;; (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (sp-pair "{" nil :post-handlers
	   '(((lambda (&rest _ignored)
		(crux-smart-open-line-above)) "RET")))
  (sp-with-modes '(org-mode)
    (sp-local-pair "*" "*"))
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

(provide 'core-smartparens)
;;; module-smartparens ends here
