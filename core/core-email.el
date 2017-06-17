;;; core-email -- Email
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package mu4e
  :ensure nil
  :init
  (setq-default mu4e-maildir          (expand-file-name "~/Mail/elektronaut")
                mu4e-drafts-folder    "/Drafts"
                mu4e-sent-folder      "/Sent Messages"
                mu4e-trash-folder     "/Trash"
                mu4e-get-mail-command "offlineimap"
                ;;mu4e-sent-messages-behavior 'delete
                mu4e-maildir-shortcuts '(("/"              . ?i)
                                         ("/Sent Messages" . ?s)
                                         ("/Trash"         . ?t))))

(provide 'core-email)
;;; core-email ends here
