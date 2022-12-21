;;; core-email -- Email
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'smtpmail)

(use-package mu4e
  :ensure nil
  :defer 20
  :commands (mu4e)
  :config

  (require 'mu4e-org)

  ;; Use mu4e for composing email
  (setq mail-user-agent 'mu4e-user-agent)

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context if no context matches;
  ;; default is to ask
  (setq mu4e-compose-context-policy nil)

  ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'sent)

  ;; UI
  (setq mu4e-main-hide-personal-addresses t
        mu4e-completing-read-function 'completing-read
        mu4e-headers-visible-lines 15
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject)))

  ;; Markers
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark     '("D" . "D")
        mu4e-headers-flagged-mark   '("F" . "+")
        mu4e-headers-new-mark       '("N" . "N")
        mu4e-headers-passed-mark    '("P" . "❯")
        mu4e-headers-replied-mark   '("R" . "❮")
        mu4e-headers-seen-mark      '("S" . "S")
        mu4e-headers-trashed-mark   '("T" . "T")
        mu4e-headers-attach-mark    '("a" . "a")
        mu4e-headers-encrypted-mark '("x" . "x")
        mu4e-headers-signed-mark    '("s" . "s")
        mu4e-headers-unread-mark    '("u" . "u")
        mu4e-headers-list-mark      '("s" . "s")
        mu4e-headers-personal-mark  '("p" . "")
        mu4e-headers-calendar-mark  '("c" . "c"))

  ;; Hide messages from spam folder
  ;; (setq mu4e-headers-hide-predicate
  ;;       (lambda (msg)
  ;;         (string-suffix-p "Spam" (mu4e-message-field msg :maildir))))

  ;; Directories
  (setq mu4e-maildir (expand-file-name "~/Mail")
        mu4e-attachment-dir "~/Downloads")

  ;; Fetching
  (setq mu4e-get-mail-command "/usr/local/bin/mbsync -a"
        mu4e-update-interval (* 60 10)
        mu4e-hide-index-messages t)

  ;; mbsync needs a file rename when file is moved.
  (setq mu4e-change-filenames-when-moving t)

  ;; Composing
  (setq mu4e-compose-format-flowed nil)
  (add-hook 'mu4e-compose-mode-hook 'visual-fill-column-mode)
  (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)

  ;; SMTP config
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587)

  ;; SMTP queueing
  (setq smtpmail-queue-mail t
        smtpmail-queue-dir "~/Mail/queue/cur")

  ;; Org capture actions
  (setq mu4e-org-link-query-in-headers-mode nil)

  (defun my/capture-mail-follow-up (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mf"))

  (defun my/capture-mail-read-later (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mr"))

  (defun org-store-link-to-mu4e-query ()
    "Stores a link to the current mu4e query."
    (interactive)
    (let ((mu4e-org-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  (add-to-list 'mu4e-headers-actions
               '("follow up" . my/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
               '("follow up" . my/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
               '("read later" . my/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
               '("read later" . my/capture-mail-read-later) t)

  ;; Mail accounts
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Anyone"
             :enter-func (lambda () (mu4e-message "Entering Anyone context"))
             :match-func (lambda (msg)
                           (when msg (string-match-p "^/anyone"
                                                     (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address	        . "inge@anyone.no")
                     (user-full-name	        . "Inge Jørgensen")
                     (message-user-organization . "Anyone")
                     (mu4e-compose-signature    . nil)
                     (smtpmail-smtp-user        . "inge@anyone.no")
                     (smtpmail-smtp-server      . "smtp.gmail.com")
                     (mu4e-sent-messages-behavior . delete)
                     (my-org-capture-email-target . "~/Library/CloudStorage/Dropbox/org/anyone.org")
                     (mu4e-refile-folder . "/anyone/[Gmail]/All Mail")
                     (mu4e-drafts-folder . "/anyone/[Gmail]/Drafts")
                     (mu4e-sent-folder   . "/anyone/[Gmail]/Sent Mail")
                     (mu4e-trash-folder  . "/anyone/[Gmail]/Trash")
                     (mu4e-maildir-shortcuts . (("/anyone/INBOX" . ?i)
                                                ("/anyone/Alerts" . ?A)
                                                ("/anyone/GitHub" . ?G)
                                                ("/anyone/[Gmail]/All Mail" . ?a)
                                                ("/anyone/[Gmail]/Drafts" . ?d)
                                                ("/anyone/[Gmail]/Sent Mail" . ?s)))))

           ,(make-mu4e-context
             :name "Elektronaut"
             :enter-func (lambda () (mu4e-message "Entering Elektronaut context"))
             :match-func (lambda (msg)
                           (when msg (string-match-p "^/elektronaut"
                                                     (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address	        . "inge@elektronaut.no")
                     (user-full-name	        . "Inge Jørgensen")
                     (message-user-organization . nil)
                     (mu4e-compose-signature    . nil)
                     (smtpmail-smtp-user        . "inge@elektronaut.no")
                     (smtpmail-smtp-server      . "smtp.gmail.com")
                     (mu4e-sent-messages-behavior . delete)
                     (my-org-capture-email-target . "~/Library/CloudStorage/Dropbox/org/personal.org")
                     (mu4e-refile-folder . "/elektronaut/[Gmail]/All Mail")
                     (mu4e-drafts-folder . "/elektronaut/[Gmail]/Drafts")
                     (mu4e-sent-folder   . "/elektronaut/[Gmail]/Sent Mail")
                     (mu4e-trash-folder  . "/elektronaut/[Gmail]/Trash")
                     (mu4e-maildir-shortcuts . (("/elektronaut/INBOX" . ?i)
                                                ("/elektronaut/[Gmail]/All Mail" . ?a)
                                                ("/elektronaut/[Gmail]/Drafts" . ?d)
                                                ("/elektronaut/[Gmail]/Sent Mail" . ?s)))))

           ,(make-mu4e-context
             :name "iCloud"
             :enter-func (lambda () (mu4e-message "Entering iCloud context"))
             :match-func (lambda (msg)
                           (when msg (string-match-p "^/icloud"
                                                     (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address	        . "ingej@me.com")
                     (user-full-name	        . "Inge Jørgensen")
                     (message-user-organization . nil)
                     (mu4e-compose-signature    . nil)
                     (smtpmail-smtp-user        . "ingej")
                     (smtpmail-smtp-server      . "smtp.mail.me.com")
                     (my-org-capture-email-target . "~/Library/CloudStorage/Dropbox/org/personal.org")
                     (mu4e-refile-folder . "/icloud/Archive")
                     (mu4e-drafts-folder . "/icloud/Drafts")
                     (mu4e-sent-folder   . "/icloud/Sent Messages")
                     (mu4e-trash-folder  . "/icloud/Deleted Messages")
                     (mu4e-maildir-shortcuts . (("/icloud/INBOX" . ?i)
                                                ("/icloud/Archive" . ?a)
                                                ("/icloud/Drafts" . ?d)
                                                ("/icloud/Sent Messages" . ?s)))))))

  ;; Bookmarks

  (defun maildir-or (mailboxes)
    (string-join (mapcar (lambda (b) (concat "\"maildir:" b "\""))
                         mailboxes) " OR "))

  (defvar inbox-folders
    (maildir-or '("/anyone/INBOX"
                  "/elektronaut/INBOX"
                  "/icloud/INBOX")))

  (defvar spam-folders
    (maildir-or '("/anyone/[Gmail]/Spam"
                  "/elektronaut/[Gmail]/Spam"
                  "/icloud/Junk")))

  (defun without-spam (query)
    (format "NOT (%s) AND (%s)" spam-folders query))

  (setq mu4e-bookmarks
        (list
         `(,(without-spam
             (string-join
              (list inbox-folders
                    "(flag:unread OR flag:flagged) AND NOT flag:trashed")
              " OR "))
           "Dashboard" ?d)
         `(,inbox-folders "All inboxes" ?i)
         `(,(without-spam "flag:unread AND NOT flag:trashed") "Unread messages" ?u)
         `(,(without-spam "date:today..now") "Today's messages" ?t)
         `(,(without-spam "date:7d..now") "Last 7 days" ?w)
         `(,(without-spam "flag:flagged") "Flagged" ?f)
         `(,(without-spam "mime:image/*") "Messages with images" ?p)
         `(,(maildir-or '("/anyone/[Gmail]/Drafts"
                          "/elektronaut/[Gmail]/Drafts"
                          "/icloud/Drafts")) "Drafts" ?d)
         `(,(maildir-or '("/anyone/[Gmail]/Sent Mail"
                          "/elektronaut/[Gmail]/Sent Mail"
                          "/icloud/Sent Messages")) "Sent messages" ?s)
         `(,spam-folders "Spam" ?S)
         `(,(maildir-or '("/anyone/[Gmail]/Trash"
                          "/elektronaut/[Gmail]/Trash"
                          "/icloud/Deleted Messages")) "Trash" ?T)
         `("maildir:/queue" "Queue" ?Q)))

  ;; Run mu4e in the background
  (mu4e t))


(provide 'core-email)
;;; core-email.el ends here
