;;; core-secrets.el --- Secrets
;;; Commentary:
;;; Code:

(defcustom config-secrets-alist '()
  "Add all your config secrets to this."
  :type 'alist
  :group 'secrets)

(defcustom config-secrets-file
  (expand-file-name "secrets.el" user-emacs-directory)
  "File for storing secrets."
  :type :string
  :group 'secrets)

(defun config-secret (key)
  "Get secret value for KEY, or nil if it isn't defined."
  (alist-get key config-secrets-alist))

;; Load the secrets file
(if (file-exists-p config-secrets-file)
    (load config-secrets-file))

(provide 'core-secrets)
;;; core-secrets.el ends here
