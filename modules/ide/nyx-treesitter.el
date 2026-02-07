;;; nyx-treesitter.el --- Tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :config
  ;; treesit-language-available-p cache. See:
  ;; https://github.com/renzmann/treesit-auto/issues/135#issuecomment-3314598444
  (defvar nyx-treesit-lang-cache
    (make-hash-table :test 'equal)
    "Cache the expensive computation of treelit language availability.
     See `nyx-treesit-language-available-p' for usage.")
  (defun nyx-treesit-language-available-p (fn lang &rest rest)
    "Caching around the CPU expensive `treesit-language-available-p'."
    (let ((cached-value
           (gethash lang nyx-treesit-lang-cache 'miss)))
      (if (eq 'miss cached-value)
          (let ((value
                 (apply fn lang rest)))
            (puthash lang value nyx-treesit-lang-cache)
            value)
        cached-value)))
  (advice-add #'treesit-language-available-p
              :around #'nyx-treesit-language-available-p))

;; Automatic installation, usage, and fallback for tree-sitter
;; major modes.
(use-package treesit-auto
  :ensure t
  :demand t
  :commands (global-treesit-auto-mode)
  :custom ((treesit-auto-install 'prompt))
  :config
  (global-treesit-auto-mode))

(provide 'nyx-treesitter)
;;; nyx-treesitter.el ends here
