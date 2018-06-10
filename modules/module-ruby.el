;;; module-ruby -- Ruby config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

;; Ignore Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(use-package bundler)

(use-package rbenv
  :config
  (global-rbenv-mode))

(use-package enh-ruby-mode
  :config
  (setq-default enh-ruby-bounce-deep-indent t
                enh-ruby-hanging-indent-level 2)
  (add-hook 'enh-ruby-mode-hook 'subword-mode))

(use-package ruby-mode
  :defer t
  :ensure nil
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
  (setq ruby-use-smie t
        ruby-align-chained-calls t
        ruby-align-to-stmt-keywords nil
        ruby-custom-encoding-magic-comment-template "# encoding: %s"
        ruby-deep-indent-paren t)
  (add-hook 'ruby-mode-hook 'subword-mode))

(use-package inf-ruby
  :config
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package robe
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package rspec-mode
  :config
  (rspec-install-snippets))

(use-package rubocop
  :config
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(use-package ruby-tools
  :config
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'ruby-tools-mode))

(use-package projectile-rails
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package slim-mode :mode "\\.slim\\'")

(use-package yari
  :init
  (define-key 'help-command (kbd "R") 'yari))

(provide 'module-ruby)
;;; module-ruby ends here
