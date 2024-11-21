;;; nyx-flyspell.el --- Flyspell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flyspell
  :ensure t
  :demand t
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :commands (flyspell-correct-word)
  :config
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(provide 'nyx-flyspell)
;;; nyx-flyspell.el ends here
