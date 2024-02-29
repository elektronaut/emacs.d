;;; module-ruby -- Ruby config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

;; Ignore Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(use-package bundler
  :ensure t
  :defer 10)

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
  "\\.builder\\'"
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
        ruby-insert-encoding-magic-comment nil
        ruby-deep-indent-paren t)
  (add-hook 'ruby-mode-hook 'subword-mode))

;; (use-package rbenv
;;   :ensure t
;;   :after (ruby-mode)
;;   :config
;;   (global-rbenv-mode))

(use-package enh-ruby-mode
  :ensure t
  :after (ruby-mode)
  :config
  (setq-default enh-ruby-bounce-deep-indent t
                enh-ruby-hanging-indent-level 2)
  (add-hook 'enh-ruby-mode-hook 'subword-mode))

(use-package inf-ruby
  :ensure t
  :after (enh-ruby-mode)
  :config
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package robe
  :ensure t
  :after (enh-ruby-mode)
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package rspec-mode
  :ensure t
  :after (ruby-mode)
  :config
  (rspec-install-snippets))

(use-package rubocop
  :ensure t
  :after (enh-ruby-mode)
  :config
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(use-package ruby-tools
  :ensure t
  :after (enh-ruby-mode)
  :config
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'ruby-tools-mode))

(use-package yari
  :ensure t
  :after (ruby-mode)
  :init
  (define-key 'help-command (kbd "R") 'yari))

(use-package projectile-rails
  :ensure t
  :after (ruby-mode projectile)
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(provide 'module-ruby)
;;; module-ruby ends here
