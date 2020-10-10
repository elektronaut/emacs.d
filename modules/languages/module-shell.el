;;; module-shell -- Shell
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package sh-script)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'module-shell)
;;; module-shell ends here
