;;; core-tramp-server -- Tramp server
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'dash)

(defvar core-tramp-server-port 9999
  "port of the echo server")

(defvar core-tramp-server-clients '()
  "alist where KEY is a client process and VALUE is the string")

(defun core-tramp-server-start nil
  "starts an emacs echo server"
  (interactive)
  (unless (process-status "core-tramp-server")
    (make-network-process
     :name "core-tramp-server"
     :host 'local
     :buffer "*core-tramp-server*"
     :family 'ipv4
     :service core-tramp-server-port
     :sentinel 'core-tramp-server-sentinel
     :filter 'core-tramp-server-filter
     :server 't)
    (setq core-tramp-server-clients '())
    (core-tramp-server-log
     (propertize (concat "Server started, listening on port "
                         (number-to-string core-tramp-server-port))
                 'face 'font-lock-builtin-face))))

(defun core-tramp-server-stop nil
  "stop an emacs core-tramp server"
  (interactive)
  (core-tramp-server-log
   (propertize "Server shutting down..."
               'face 'font-lock-builtin-face))
  (while core-tramp-server-clients
    (delete-process (car (car core-tramp-server-clients)))
    (setq core-tramp-server-clients (cdr core-tramp-server-clients)))
  (delete-process "core-tramp-server"))

(defun core-tramp-server-open-file (path)
  (find-file path)
  (x-focus-frame nil))

(defun core-tramp-server-perform (proc command args)
  ;;(core-tramp-server-open-file payload)
  (core-tramp-server-log command)
  (cond ((string-equal command "open")
         (mapc (lambda (p) (core-tramp-server-open-file p)) args)
         "OK")
        ((string-equal command "exit")
         (delete-process proc))
        (t (concat "Unknown command: " command))))

(defun core-tramp-server-filter (proc string)
  (let ((pending (assoc proc core-tramp-server-clients))
        message
        index)
    (unless pending
      (setq core-tramp-server-clients (cons (cons proc "") core-tramp-server-clients))
      (setq pending  (assoc proc core-tramp-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (let* ((payload (string-trim (substring message 0 index)))
             (command (car (split-string payload " ")))
             (args (--> (cdr (split-string payload " "))
                        (-remove (lambda (s) (string= "" s)) it))))
        (core-tramp-server-log payload proc)
        (process-send-string
         proc
         (core-tramp-server-perform proc command args)))
      (setq message (substring message index)))
    (setcdr pending message)))

(defun core-tramp-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq core-tramp-server-clients (assq-delete-all proc core-tramp-server-clients))
    (core-tramp-server-log
     (propertize "Client has quit"
                 'face 'font-lock-doc-face)
     proc)))

;;from server.el
(defun core-tramp-server-log (string &optional client)
  "If a *core-tramp-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*core-tramp-server*")
      (with-current-buffer "*core-tramp-server*"
        (goto-char (point-max))
        (insert
         (propertize (current-time-string) 'face 'font-lock-doc-face)
         (propertize (if client (format " %s" client) "") 'face 'font-lock-type-face)
         " "
         string)
        (or (bolp) (newline)))))

(core-tramp-server-start)

(provide 'core-tramp-server)
;;; core-tramp-server ends here
