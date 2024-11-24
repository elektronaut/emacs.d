;;; nyx-help.el --- Help -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-global-set "C-h A" 'apropos)
(keymap-global-set "C-h C-f" 'find-function)
(keymap-global-set "C-h C-k" 'find-function-on-key)
(keymap-global-set "C-h C-v" 'find-variable)
(keymap-global-set "C-h C-l" 'find-library)

(use-package discover-my-major
  :ensure t
  :bind (("C-c m" . discover-my-major)))

(provide 'nyx-help)
;;; nyx-help.el ends here
