;;; nyx-markdown.el --- Markdown -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom ((markdown-command "multimarkdown")))

(provide 'nyx-markdown)
;;; nyx-markdown.el ends here
