;;; module-python -- Python
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'module-company)
(require 'electric)

(use-package python-mode
  :mode "\\.py\\'"
  :init
  (when (fboundp 'exec-path-from-shell-copy-env)
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  :config
  (defun module-python-defaults ()
    "Defaults for Python programming."
    (subword-mode +1)
    (eldoc-mode 1)
    (setq-local electric-layout-rules
                '((?: . (lambda ()
                          (and (zerop (first (syntax-ppss)))
                               (python-info-statement-starts-block-p)
                               'after)))))
    (when (fboundp #'python-imenu-create-flat-index)
      (setq-local imenu-create-index-function
                  #'python-imenu-create-flat-index))
    (add-hook 'post-self-insert-hook
              #'electric-layout-post-self-insert-function nil 'local))
  (add-hook 'python-mode-hook 'module-python-defaults)
  (use-package anaconda-mode
    :config
    (use-package company-anaconda
      :config
      (add-to-list 'company-backends 'company-anaconda)
      (add-hook 'python-mode-hook 'anaconda-mode))))

(use-package cython-mode
  :mode "\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'")

(provide 'module-python)
;;; module-python ends here
