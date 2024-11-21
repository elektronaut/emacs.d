;;; nyx-editing.el --- Editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Align the current region using an ad-hoc rule read from the
;; minibuffer.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)

(set-default 'imenu-auto-rescan t)

;; Enable repeat-mode
(repeat-mode 1)

;; Enables moving lines or regions up and down
(use-package move-text
  :ensure t
  :config
  (global-set-key [(control shift up)]  'move-text-up)
  (global-set-key [(control shift down)]  'move-text-down)
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down))

;; Enables applying arithmetic operations on numbers in buffer
(use-package operate-on-number
  :ensure t
  :bind
  (:repeat-map operate-on-number-repeat-map
               ("+" . apply-operation-to-number-at-point)
               ("-" . apply-operation-to-number-at-point)
               ("*" . apply-operation-to-number-at-point)
               ("/" . apply-operation-to-number-at-point)
               ("\\" . apply-operation-to-number-at-point)
               ("^" . apply-operation-to-number-at-point)
               ("<" . apply-operation-to-number-at-point)
               (">" . apply-operation-to-number-at-point)
               ("#" . apply-operation-to-number-at-point)
               ("%" . apply-operation-to-number-at-point)
               ("'" . operate-on-number-at-point))
  :bind-keymap ("C-c ." . operate-on-number-repeat-map))

;; Built-in tool for interactive regexp building
(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

;; Converts between different string naming conventions
(use-package string-inflection
  :ensure t)

(provide 'nyx-editing)
;;; nyx-editing.el ends here
