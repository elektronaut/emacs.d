;;; module-erc -- ERC
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(setq erc-server-coding-system '(utf-8 . utf-8)
      erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-save-buffer-on-part t
      erc-query-display 'buffer
      erc-log-channels-directory "~/.logs/erc/"
      erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-autoaway-use-emacs-idle t
      erc-autojoin-channels-alist '(("#rubyonrails" "#ruby" "#emacs")))

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(erc-truncate-mode +1)
(erc-spelling-mode 1)

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from
the same person.")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick erc-nick)))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))

(provide 'module-erc)
;;; module-erc ends here
