;;; module-python -- Python
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'electric)

(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :config
  (when (fboundp 'exec-path-from-shell-copy-env)
    (exec-path-from-shell-copy-env "PYTHONPATH"))
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
  (add-hook 'python-mode-hook 'module-python-defaults))

(use-package anaconda-mode
  :ensure t
  :after (python-mode))

(use-package company-anaconda
  :ensure t
  :after (anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package cython-mode
  :ensure t
  :mode "\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'")

(provide 'module-python)
;;; module-python ends here
