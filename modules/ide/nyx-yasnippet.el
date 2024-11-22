;;; nyx-yasnippet.el --- yasnippet -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :custom ((yas-snippet-dirs
            (list (expand-file-name "snippets" user-emacs-directory))))
  :init
  (yas-global-mode 1))

(provide 'nyx-yasnippet)
;;; nyx-yasnippet.el ends here
