;;; nyx-erc.el --- ERC -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a notification.")

(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from the same person.")

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(use-package erc
  :defer t
  :commands (erc start-irc stop-irc erc-track-mode)
  :custom ((erc-server-coding-system '(utf-8 . utf-8))
           (erc-kill-buffer-on-part t)
           (erc-kill-queries-on-quit t)
           (erc-kill-server-buffer-on-quit t)
           (erc-save-buffer-on-part t)
           (erc-query-display 'buffer)
           (erc-auto-discard-away t)
           (erc-autoaway-idle-seconds 600))
  :functions (erc-filter-server-buffers)
  :config
  (if (not (file-exists-p erc-log-channels-directory))
      (mkdir erc-log-channels-directory t))

  (erc-track-mode t)
  (erc-truncate-mode +1)
  (erc-spelling-mode 1)

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

  (defun erc-filter-server-buffers ()
    (delq nil
          (mapcar
           (lambda (x) (and (erc-server-or-unjoined-channel-buffer-p x) x))
           (buffer-list))))

  (defun start-irc ()
    "Connect to IRC."
    (interactive)
    (when (y-or-n-p "Do you want to start IRC? ")
      (erc :server "irc.freenode.net" :port 6667 :nick erc-nick)))

  (defun stop-irc ()
    "Disconnects from all irc servers"
    (interactive)
    (dolist (buffer (erc-filter-server-buffers))
      (message "Server buffer: %s" (buffer-name buffer))
      (with-current-buffer buffer
        (erc-quit-server "Asta la vista")))))

(provide 'nyx-erc)
;;; nyx-erc.el ends here
