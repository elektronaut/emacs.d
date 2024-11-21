;;; nyx-yaml.el --- YAML -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'" "\\.yml\\'"
  :hook ((yaml-mode . whitespace-mode)
         (yaml-mode . subword-mode)))

(provide 'nyx-yaml)
;;; nyx-yaml.el ends here
