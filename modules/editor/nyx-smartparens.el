;;; nyx-smartparens.el --- Smartparens -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-crux)

(use-package smartparens
  :ensure t
  :functions sp-pair sp-local-pair sp-local-tag
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ;;("M-<up>" . sp-up-sexp)
              ;;("M-<down>" . sp-down-sexp)
              ("C-M-<up>" . sp-backward-up-sexp)
              ("C-M-<down>" . sp-backward-down-sexp)
              ("C-M-<right>" . sp-forward-sexp)
              ("C-M-<left>" . sp-backward-sexp)

              ("C-M-S-<up>" . sp-unwrap-sexp)
              ("C-M-S-<down>" . sp-wrap-round)

              ("C-S-<left>" . sp-forward-barf-sexp)
              ("C-S-<right>" . sp-forward-slurp-sexp)
              ("C-M-S-<left>" . sp-backward-slurp-sexp)
              ("C-M-S-<right>" . sp-backward-barf-sexp)

              ("C-c R" . sp-rewrap-sexp))
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (minibuffer-setup . conditionally-enable-smartparens-mode))
  :init
  (setq-default ;; sp-base-key-bindings 'paredit
   sp-autoskip-closing-pair 'always
   sp-hybrid-kill-entire-symbol nil)
  :config
  (require 'smartparens-config)

  (defun conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode 1)))


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

(provide 'nyx-smartparens)
;;; nyx-smartparens.el ends here
