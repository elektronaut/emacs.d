;;; nyx-comment.el --- Commenting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-M-,") 'comment-or-uncomment-region-or-line)

(provide 'nyx-comment)
;;; nyx-comment.el ends here
