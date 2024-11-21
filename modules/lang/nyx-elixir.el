;;; nyx-elixir.el --- Elixir -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :mode "\\.ex\\'" "\\.exs\\'" "\\.elixir\\'")

(use-package alchemist
  :ensure t
  :after (elixir-mode))

(provide 'nyx-elixir)
;;; nyx-elixir.el ends here
