;;; module-elixir -- Elixir
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package elixir-mode
  :ensure t
  :mode "\\.ex\\'" "\\.exs\\'" "\\.elixir\\'"
  :config
  (use-package alchemist))

(provide 'module-elixir)
;;; module-elixir ends here
