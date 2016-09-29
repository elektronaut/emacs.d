;;; module-go -- Go
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-company)

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
         ("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)
         ("C-h f" . godoc-at-point))
  :init
  (add-to-list 'completion-ignored-extensions ".test")
  (define-key 'help-command (kbd "G") 'godoc)
  :config
  (use-package company-go
    :config
    (set (make-local-variable 'company-backends) '(company-go)))
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup))
  (use-package go-projectile)
  (use-package gotest)
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Prefer goimports to gofmt if installed
              (let ((goimports (executable-find "goimports")))
                (when goimports
                  (setq gofmt-command goimports)))
              ;; Run gofmt on save
              (add-hook 'before-save-hook 'gofmt-before-save nil t)
              (subword-mode +1))))

(provide 'module-go)
;;; module-go ends here
