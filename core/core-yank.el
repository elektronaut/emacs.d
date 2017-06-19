;;; core-yank.el --- Yank settings
;;; Commentary:
;;; Code:

(require 'core-macros)

(defvar yank-indent-threshold 1000
  "Max threshold for automatic indentation.")

(defvar indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

;; Automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-indent-threshold)
      (indent-region beg end nil)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(use-package browse-kill-ring
  :bind (("s-y" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

(provide 'core-yank)
;;; core-yank.el ends here
