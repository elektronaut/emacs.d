;;; nyx-rainbow-mode.el --- Rainbow mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Colorizes color names and values in buffers
(use-package rainbow-mode
  :ensure t
  :config
  (defun rainbow-turn-off-words ()
    "Turn off word colours in rainbow-mode."
    (interactive)
    (font-lock-remove-keywords
     nil
     `(,@rainbow-x-colors-font-lock-keywords
       ,@rainbow-latex-rgb-colors-font-lock-keywords
       ,@rainbow-r-colors-font-lock-keywords
       ,@rainbow-html-colors-font-lock-keywords
       ,@rainbow-html-rgb-colors-font-lock-keywords))))

(provide 'nyx-rainbow-mode)
;;; nyx-rainbow-mode.el ends here
