;;; nyx-shell.el --- Shell scripts -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'nyx-shell)
;;; nyx-shell.el ends here
