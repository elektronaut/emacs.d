;;; module-markdown -- Markdown
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'module-markdown)
;;; module-markdown ends here
