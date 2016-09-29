;;; module-ruby -- Ruby config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

;; Ignore Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(use-package rbenv
  :config
  (global-rbenv-mode))

(use-package enh-ruby-mode
  :mode "Appraisals\\'"
        "Berksfile\\'"
        "Capfile\\'"
        "Gemfile\\'"
        "Guardfile\\'"
        "Podfile\\'"
        "Puppetfile\\'"
        "Rakefile\\'"
        "Thorfile\\'"
        "Vagrantfile\\'"
        "\\.axlsx\\'"
        "\\.cap\\'"
        "\\.gemspec\\'"
        "\\.gemspec\\'"
        "\\.jbuilder\\'"
        "\\.podspec\\'"
        "\\.prawn\\'"
        "\\.rabl\\'"
        "\\.rake\\'"
        "\\.rb\\'"
        "\\.ru\\'"
        "\\.thor\\'"
  :config
  (setq-default enh-ruby-bounce-deep-indent t
                enh-ruby-hanging-indent-level 2)
  ;; CamelCase aware editing
  (add-hook 'enh-ruby-mode-hook 'subword-mode)
  (use-package inf-ruby
    :config
    (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))
  (use-package robe
    :config
    (add-hook 'enh-ruby-mode-hook 'robe-mode))
  (use-package ruby-tools
    :config
    (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)))

(use-package projectile-rails
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package slim-mode :mode "\\.slim\\'")

(provide 'module-ruby)
;;; module-ruby ends here
