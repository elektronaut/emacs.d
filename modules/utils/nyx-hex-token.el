;;; nyx-hex-token.el --- Hex token -*- lexical-binding: t; -*-
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

(provide 'nyx-hex-token)
;;; nyx-hex-token.el ends here
