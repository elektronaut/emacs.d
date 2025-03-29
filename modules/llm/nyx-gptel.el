;;; nyx-gptel.el --- gptel -*- lexical-binding: t; -*-

;;; Commentary:
;;;  gptel is a simple Large Language Model chat client for Emacs,
;;;  with support for multiple models and backends.  It works in the
;;;  spirit of Emacs, available at any time and uniformly in any
;;;  buffer.

;;; Code:

(require 'org)

(use-package gptel
  :ensure t
  :bind (("C-c g g" . gptel-send)
         ("C-c g G" . gptel-menu)
         ("C-c g b" . gptel)
         ("C-c g c" . gptel-add)
         ("C-c g f" . gptel-add-file)
         ("C-c g o" . gptel-org-set-topic)
         ("C-c g O" . gptel-org-set-properties))
  :commands gptel-end-of-response
  :custom ((gptel-default-mode #'org-mode)
           (gptel-display-buffer-action '(pop-to-buffer-same-window))
           (gptel-prompt-prefix-alist
            '((markdown-mode . "## ")
              (org-mode      . "")
              (text-mode     . "## "))))
  :hook (gptel-mode . visual-line-mode)
  :config
  (setq gptel-model 'claude-3-7-sonnet-20250219
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (auth-info-password
                              (car (auth-source-search
                                    :host "api.anthropic.com"
                                    :user "apikey")))))
  (defun gptel-end-of-response-with-heading (&optional _ _ _arg)
    "Go to end of response and insert heading"
    (interactive)
    (gptel-end-of-response)
    (if (derived-mode-p 'org-mode)
        (org-insert-heading-respect-content)))
  ;; (add-hook 'gptel-post-response-functions
  ;;           'gptel-end-of-response-with-heading)
  )

(provide 'nyx-gptel)
;;; nyx-gptel.el ends here
