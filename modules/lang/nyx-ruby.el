;;; nyx-ruby.el --- Ruby -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'no-littering)

(defun format-binary (number)
  "Convert NUMBER to binary string representation."
  (let ((result ""))
    (while (> number 0)
      (setq result (concat (number-to-string (mod number 2)) result)
            number (/ number 2)))
    (if (string= result "")
        "0"
      result)))

(defun ruby-cycle-number-base ()
  "Cycle an integer literal between decimal, hex, binary and octal representations."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (num (cond
               ((string-match "\\`0x\\([0-9a-fA-F]+\\)\\'" text)
                (string-to-number (match-string 1 text) 16))
               ((string-match "\\`0b\\([01]+\\)\\'" text)
                (string-to-number (match-string 1 text) 2))
               ((string-match "\\`0\\([0-7]+\\)\\'" text)
                (string-to-number (match-string 1 text) 8))
               ((string-match "\\`\\([0-9]+\\)\\'" text)
                (string-to-number text 10))
               (t (user-error "No number at point"))))
         (next (cond
                ((string-prefix-p "0x" text) (concat "0b" (format-binary num)))
                ((string-prefix-p "0b" text) (format "0%o" num))
                ((string-prefix-p "0" text) (number-to-string num))
                (t (format "0x%x" num)))))
    (delete-region (car bounds) (cdr bounds))
    (insert next)))

;; Ignore Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(use-package ruby-mode
  :ensure nil
  :bind (("C-c C-t" . ruby-cycle-number-base))
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

(use-package rake
  :ensure t
  :custom ((rake-cache-file (no-littering-expand-var-file-name "rake.cache"))))

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
  (keymap-global-set "C-h R" 'yari))

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
