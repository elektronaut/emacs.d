;;; nyx-movement.el --- Movement -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-global-set "<home>" 'move-beginning-of-line)
(keymap-global-set "<end>" 'move-end-of-line)

;; More mac-like movement keys
(keymap-global-set "M-<up>" 'beginning-of-buffer)
(keymap-global-set "M-<down>" 'end-of-buffer)
(keymap-global-set "M-<left>" 'move-beginning-of-line)
(keymap-global-set "M-<right>" 'move-end-of-line)
(keymap-global-set "A-<left>" 'left-word)
(keymap-global-set "A-<right>" 'right-word)

;; Quick navigation by jumping to characters
(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-char)
         ("C-c J" . avy-goto-word-or-subword-1)
         ("s-."   . avy-goto-word-or-subword-1))
  :init
  (setq avy-background t
        avy-style 'at-full))

(provide 'nyx-movement)
;;; nyx-movement.el ends here
