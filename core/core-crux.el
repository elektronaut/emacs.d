;;; core-crux -- Crux
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package crux
  :bind (("M-j"     . crux-top-join-line)
         ("C-c o"   . crux-open-with)
         ("C-a"     . crux-move-beginning-of-line)
         ("M-o"     . crux-smart-open-line)
         ("C-c n"   . crux-cleanup-buffer-or-region)
         ("C-c f"   . crux-recentf-ido-find-file)
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
         ("s-r"     . crux-recentf-ido-find-file)
         ("s-j"     . crux-top-join-line)
         ("s-k"     . crux-kill-whole-line))
  :config
  (global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key [(control shift return)] 'crux-smart-open-line-above)
  (crux-with-region-or-line kill-region))

(provide 'core-crux)
;;; core-crux ends here
