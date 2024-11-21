;;; nyx-crux.el --- Crux -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package crux
  :ensure t
  :defer 10
  :bind (("M-j"     . crux-top-join-line)
         ("C-c O"   . crux-open-with)
         ;; ("C-a"     . crux-move-beginning-of-line)
         ;; ("<home>"  . crux-move-beginning-of-line)
         ("M-o"     . crux-smart-open-line)
         ("C-c n"   . crux-cleanup-buffer-or-region)
         ("C-M-z"   . crux-indent-defun)
         ("C-c u"   . crux-view-url)
         ("C-c e"   . crux-eval-and-replace)
         ("C-c s"   . crux-swap-windows)
         ("C-c D"   . crux-delete-file-and-buffer)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c r"   . crux-rename-buffer-and-file)
         ;; ("C-c t"   . crux-visit-term-buffer)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I"   . crux-find-user-init-file)
         ("C-c S"   . crux-find-shell-init-file)
         ("s-o"     . crux-smart-open-line-above)
         ("s-j"     . crux-top-join-line)
         ("s-k"     . crux-kill-whole-line))
  :config
  (global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
  (global-set-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key [(control shift return)] 'crux-smart-open-line-above)
  (crux-with-region-or-line kill-region))

(provide 'nyx-crux)
;;; nyx-crux.el ends here
