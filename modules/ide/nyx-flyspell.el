;;; nyx-flyspell.el --- Flyspell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flyspell
  :ensure nil
  :demand t
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :commands (flyspell-correct-word)
  :config
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (keymap-set flyspell-mouse-map "<down-mouse-3>" 'flyspell-correct-word)
  (keymap-unset flyspell-mouse-map "<mouse-3>"))

(provide 'nyx-flyspell)
;;; nyx-flyspell.el ends here
