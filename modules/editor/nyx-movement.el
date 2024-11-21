;;; nyx-movement.el --- Movement -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; More mac-like movement keys
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-<right>") 'move-end-of-line)
(global-set-key (kbd "A-<left>") 'left-word)
(global-set-key (kbd "A-<right>") 'right-word)

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
