;;; core-defuns.el --- Misc functions
;;; Commentary:
;;; Code:

(defun random-hex-token (length)
  "Generate a random hexadecimal token of LENGTH using OpenSSL."
  (interactive "sLength (Default 16): ")
  (let* ((len (if (string= "" length) "16" length))
         (token (replace-regexp-in-string
                 "\n\\'" ""
                 (shell-command-to-string (concat "openssl rand -hex " len)))))
    (if (and transient-mark-mode mark-active)
        (delete-region (region-beginning) (region-end)))
    (insert token)))

(defun split-n (n)
  "Split frame in N windows."
  (let ((split-count (- n (count-windows))))
    (if (> split-count 0) (dotimes (i split-count) (split-window-right)))
    (if (< split-count 0) (dotimes (i (abs split-count))
                            (other-window -1)
                            (delete-window)
                            (other-window 1)))
    (unless (eq split-count 0) (balance-windows))))

(defun split-2 () "Split frame in 2 windows." (interactive) (split-n 2))
(defun split-3 () "Split frame in 3 windows." (interactive) (split-n 3))

(defun auto-window-layout ()
  "Automatically layout frame in 2 or 3 windows depending on size."
  (interactive)
  (if (> (frame-width) (* 3 84))
      (split-n 3)
      (split-n 2))
  (balance-windows))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (when persp-mode
    (persp-switch "none")
    (mapc 'persp-kill (remove "none" (persp-names))))
  (mapc 'kill-buffer (remove (get-buffer "*core-tramp-server*") (buffer-list)))
  (switch-to-buffer "*scratch*"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let ((keep-buffer (current-buffer))
        (persp-name (if persp-mode (safe-persp-name (get-current-persp)))))
    (when persp-name
      (persp-switch "none")
      (mapc 'persp-kill (remove persp-name (remove "none" (persp-names)))))
    (mapc 'kill-buffer (remove keep-buffer (remove (get-buffer "*core-tramp-server*") (buffer-list))))
    (if persp-name (persp-switch persp-name))
    (switch-to-buffer keep-buffer)))

(defun kill-everything ()
  "Kill all buffers and windows."
  (interactive)
  (kill-all-buffers)
  (delete-other-windows))

(defun region-to-hexcol ()
  (interactive)
  (let
      ((start (region-beginning))
       (end (region-end))
       (text))

    (setq text (buffer-substring-no-properties start end))

    (when (string-match "^[[:digit:]]+$" text)
      (setq text (format "%02x" (string-to-number text)))
      (delete-region start end)
      (insert text))))

(defun rgb-to-hex ()
  (interactive)

  (let
      ((start (region-beginning))
       (end (region-end)))

    (goto-char start)
    (set-mark start)
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (setq end (point))
    (goto-char start)

    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "[, ]" nil t) (replace-match "" nil t)))))

(provide 'core-defuns)
;;; core-defuns.el ends here
