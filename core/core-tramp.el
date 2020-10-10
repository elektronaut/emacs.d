;;; core-tramp -- Tramp
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'dash)

(use-package tramp
  :ensure nil
  :init
  (setq-default tramp-use-ssh-controlmaster-options nil
                tramp-default-method "ssh")
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(defvar tramp-ssh-history nil
  "History for tramp-ssh.")

(defvar tramp-ssh-host-cache nil
  "Cache for SSH candidates.")

(defun tramp-ssh-host-candidates ()
  "Return a list of host names from ~/.ssh/known_hosts."
  (let ((known-hosts "~/.ssh/known_hosts")
        (ip-regex "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+"))
    (if (file-exists-p known-hosts)
        (--> (with-temp-buffer
               (insert-file-contents known-hosts)
               (split-string (buffer-string) "\n" t))
             (-map (lambda (l) (split-string (car (split-string l " ")) ",")) it)
             -flatten
             -distinct
             (-remove (lambda (h) (string-match-p ip-regex h)) it)
             (-sort 'string-lessp it)))))

(defun tramp-ssh-host-function (str &rest _u)
  "Return a list of host names matching STR."
  (unless tramp-ssh-host-cache
    (setq-default tramp-ssh-host-cache (tramp-ssh-host-candidates)))
  (let* ((user (if (string-match-p "@" str)
                   (car (split-string str "@"))))
         (host (if (string-match-p "@" str)
                   (car (reverse (split-string str "@")))
                 str)))
    (--> tramp-ssh-host-cache
         (-filter (lambda (h) (string-match-p host h)) it)
         (-map (lambda (h) (if user (concat user "@" h) h)) it))))

(defun tramp-ssh-read-host ()
  "SSH host name prompt."
  (setq-default tramp-ssh-host-cache (tramp-ssh-host-candidates))
  (ivy-read "(username@)host: " #'tramp-ssh-host-function
            :dynamic-collection t
            :history 'tramp-ssh-history))

(defun tramp-ssh ()
  "Open a tramp session."
  (interactive)
  (let ((full-host (tramp-ssh-read-host)))
    (unless (equal "" full-host)
      (find-file (concat "/ssh:" full-host ":~")))))

(defun tramp-sudo-ssh ()
  "Open a tramp session."
  (interactive)
  (let* ((full-host (tramp-ssh-read-host))
         (user (if (string-match-p "@" full-host)
                   (car (split-string full-host "@"))))
         (host (if (string-match-p "@" full-host)
                   (car (reverse (split-string full-host "@")))
                 full-host))
         (connect-str (if user (concat "/ssh:" user "@" host "|sudo:root@" host ":~")
                        (concat "/ssh:" user host "|sudo:root@" host  ":~"))))
    (unless (equal "" full-host)
      (find-file connect-str))))

(provide 'core-tramp)
;;; core-tramp ends here
