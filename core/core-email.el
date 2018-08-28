;;; core-email -- Email
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package mu4e
  :commands (mu4e)
  :ensure nil
  :config
  (setq-default mu4e-maildir          (expand-file-name "~/Mail/anyone")
                mu4e-drafts-folder    "/drafts"
                mu4e-sent-folder      "/sent"
                mu4e-trash-folder     "/trash"
                mu4e-get-mail-command "mbsync -a"
                ;;mu4e-sent-messages-behavior 'delete
                mu4e-maildir-shortcuts '(("/"              . ?i)
                                         ("/sent" . ?s)
                                         ("/trash"         . ?t))))

(provide 'core-email)
;;; core-email ends here
