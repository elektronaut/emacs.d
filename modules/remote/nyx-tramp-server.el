;;; nyx-tramp-server.el --- Tramp server -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package provides a simple TCP server that allows external programs to
;; remotely open files in Emacs.  The server listens on port 9999 (by default)
;; and accepts newline-terminated commands.
;;
;; Supported commands:
;; - open [filepath ...] : Opens one or more files in Emacs
;; - exit               : Disconnects the client
;;
;; The server maintains a log buffer *nyx-tramp-server* where you can monitor
;; connections and commands.  This is particularly useful for debugging and
;; monitoring server activity.
;;
;; Example client connection using netcat:
;;   echo "open /path/to/file.txt" | nc localhost 9999

;;; Code:

(require 'dash)

(defvar nyx-tramp-server-port 9999
  "Port of the nyx-tramp server.")

(defvar nyx-tramp-server-clients '()
  "Association list of active client connections.
Each element is a cons cell (PROCESS . PENDING-INPUT) where:
- PROCESS is the client process object
- PENDING-INPUT is partially received data waiting for newline")

(defun nyx-tramp-server-start nil
  "Start the nyx-tramp TCP server.
Initializes a network process listening on `nyx-tramp-server-port'.
Creates a log buffer *nyx-tramp-server* for monitoring connections.
Does nothing if server is already running."
  (interactive)
  (unless (process-status "nyx-tramp-server")
    (make-network-process
     :name "nyx-tramp-server"
     :host 'local
     :buffer "*nyx-tramp-server*"
     :family 'ipv4
     :service nyx-tramp-server-port
     :sentinel 'nyx-tramp-server-sentinel
     :filter 'nyx-tramp-server-filter
     :server 't)
    (setq nyx-tramp-server-clients '())
    (set-process-query-on-exit-flag (get-process "nyx-tramp-server") nil)
    (nyx-tramp-server-log
     (propertize (concat "Server started, listening on port "
                         (number-to-string nyx-tramp-server-port))
                 'face 'font-lock-builtin-face))))

(defun nyx-tramp-server-stop nil
  "Stop the nyx-tramp TCP server.
Disconnects all client connections and terminates the server process.
Leaves the log buffer intact but stops accepting new connections."
  (interactive)
  (nyx-tramp-server-log
   (propertize "Server shutting down..."
               'face 'font-lock-builtin-face))
  (while nyx-tramp-server-clients
    (delete-process (car (car nyx-tramp-server-clients)))
    (setq nyx-tramp-server-clients (cdr nyx-tramp-server-clients)))
  (delete-process "nyx-tramp-server"))

(defun nyx-tramp-server-open-file (path)
  "Open file at PATH and focus the Emacs frame."
  (find-file path)
  (x-focus-frame nil))

(defun nyx-tramp-server-perform (proc command args)
  "Execute COMMAND with ARGS received from client process PROC.
COMMAND can be `open' to open files or `exit' to disconnect client.
Returns response string to be sent back to client."
  ;;(nyx-tramp-server-open-file payload)
  (nyx-tramp-server-log command)
  (cond ((string-equal command "open")
         (mapc (lambda (p) (nyx-tramp-server-open-file p)) args)
         "OK")
        ((string-equal command "exit")
         (delete-process proc))
        (t (concat "Unknown command: " command))))

(defun nyx-tramp-server-filter (proc string)
  "Process filter function for handling input from clients.
PROC is the client process, STRING is the received input.
Accumulates partial messages until a newline is received, then
processes the complete command."
  (let ((pending (assoc proc nyx-tramp-server-clients))
        message
        index)
    (unless pending
      (setq nyx-tramp-server-clients (cons (cons proc "") nyx-tramp-server-clients))
      (setq pending  (assoc proc nyx-tramp-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (let* ((payload (string-trim (substring message 0 index)))
             (command (car (split-string payload " ")))
             (args (--> (cdr (split-string payload " "))
                        (-remove (lambda (s) (string= "" s)) it))))
        (nyx-tramp-server-log payload proc)
        (process-send-string
         proc
         (nyx-tramp-server-perform proc command args))
        )
      (setq message (substring message index)))
    (setcdr pending message)))

(defun nyx-tramp-server-sentinel (proc msg)
  "Process sentinel function for handling client connections.
PROC is the client process, MSG is the status message.
Cleans up client state when connection is closed."
  (when (string= msg "connection broken by remote peer\n")
    (setq nyx-tramp-server-clients (assq-delete-all proc nyx-tramp-server-clients))
    (nyx-tramp-server-log
     (propertize "Client has quit"
                 'face 'font-lock-doc-face)
     proc)))

;;from server.el
(defun nyx-tramp-server-log (string &optional client)
  "Log a message to the server buffer.
STRING is the message to log.
CLIENT, if provided, is the client process to include in the log entry."
  (if (get-buffer "*nyx-tramp-server*")
      (with-current-buffer "*nyx-tramp-server*"
        (goto-char (point-max))
        (insert
         (propertize (current-time-string) 'face 'font-lock-doc-face)
         (propertize (if client (format " %s" client) "") 'face 'font-lock-type-face)
         " "
         string)
        (or (bolp) (newline)))))

(nyx-tramp-server-start)

(provide 'nyx-tramp-server)
;;; nyx-tramp-server.el ends here
