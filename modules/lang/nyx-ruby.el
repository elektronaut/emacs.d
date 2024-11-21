;;; nyx-ruby.el --- Ruby -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Ignore Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(use-package ruby-mode
  :ensure nil
  :mode ("Appraisals\\'" "Berksfile\\'" "Capfile\\'" "Gemfile\\'"
         "Guardfile\\'" "Podfile\\'" "Puppetfile\\'" "Rakefile\\'"
         "Thorfile\\'" "Vagrantfile\\'"
         "\\.axlsx\\'" "\\.builder\\'" "\\.cap\\'" "\\.gemspec\\'"
         "\\.jbuilder\\'" "\\.podspec\\'" "\\.prawn\\'" "\\.rabl\\'"
         "\\.rake\\'" "\\.rb\\'" "\\.ru\\'" "\\.thor\\'")
  :custom ((ruby-align-chained-calls t)
           (ruby-align-to-stmt-keywords nil)
           (ruby-custom-encoding-magic-comment-template "# encoding: %s")
           (ruby-insert-encoding-magic-comment nil)
           (ruby-deep-indent-paren t))
  :hook (ruby-mode . subword-mode))

;; Interface for Ruby's Bundler package manager
(use-package bundler
  :ensure t
  :after (ruby-mode))

;; REPL for interactive Ruby development
(use-package inf-ruby
  :ensure t
  :hook ((ruby-mode    . inf-ruby-minor-mode)
         (ruby-ts-mode . inf-ruby-minor-mode)))

;; Code navigation and documentation
(use-package robe
  :ensure t
  :hook ((ruby-mode    . robe-mode)
         (ruby-ts-mode . robe-mode)))

;; Interface for running RSpec tests
(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode    . rspec-mode)
         (ruby-ts-mode . rspec-mode))
  :functions (rspec-install-snippets)
  :config
  (rspec-install-snippets))

;; Integration with RuboCop style checker
(use-package rubocop
  :ensure t
  :after (ruby-mode)
  :hook ((ruby-mode    . rubocop-mode)
         (ruby-ts-mode . rubocop-mode)))

;; Collection of handy functions for Ruby development
(use-package ruby-tools
  :ensure t
  :hook ((ruby-mode    . ruby-tools-mode)
         (ruby-ts-mode . ruby-tools-mode)))

;; Yet Another RI Interface (Ruby documentation lookup)
(use-package yari
  :ensure t
  :after (ruby-mode)
  :init
  (define-key 'help-command (kbd "R") 'yari))

;; Rails-specific features for Projectile project manager
(use-package projectile-rails
  :ensure t
  :after (projectile)
  :hook ((projectile-mode . projectile-rails-on)))

;; Major mode for editing HAML templates
(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

;; Major mode for editing Slim templates
(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(provide 'nyx-ruby)
;;; nyx-ruby.el ends here
