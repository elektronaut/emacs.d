;;; module-gptel -- gptel: A simple LLM client for Emacs
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package gptel
  :bind
  (("C-c g g" . gptel-send)
   ("C-c g G" . gptel-menu)
   ("C-c g b" . gptel)
   ("C-c g c" . gptel-add)
   ("C-c g f" . gptel-add-file)
   ("C-c g o" . gptel-org-set-topic)
   ("C-c g O" . gptel-org-set-properties))
  :config
  (setq gptel-default-mode #'org-mode
        gptel-prompt-prefix-alist
        '((markdown-mode . "# ")
          (org-mode . "* ")
          (text-mode . "# ")))
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (auth-info-password
                (car (auth-source-search
                      :host "api.anthropic.com"
                      :user "apikey")))))
  (add-hook 'gptel-mode-hook #'visual-line-mode)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'module-gptel)
;;; module-gptel.el ends here
