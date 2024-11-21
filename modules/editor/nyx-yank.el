;;; nyx-yank.el --- Yanking -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq kill-ring-max 1024)

;; Browse kill ring
(use-package browse-kill-ring
  :ensure t
  :bind (("s-y" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

;; Improves built-in kill commands with better text selection
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; Indent code when yanked
(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format '(untabify delete-trailing-whitespace))
           (snap-indent-skip-on-prefix-arg t)
           ;; Also indent on save
           (snap-indent-on-save t)))

(provide 'nyx-yank)
;;; nyx-yank.el ends here
