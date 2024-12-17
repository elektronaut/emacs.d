;;; nyx-modeline.el --- Mode line -*- lexical-binding: t; -*-

;;; Commentary:
;;    Based on doom-themes by Henrik Lissner <henrik@lissner.net>
;;    https://github.com/hlissner/.emacs.d

;;; Code:

(require 'nyx-modeline-faces)

;; Selected window

(defvar nyx-modeline-active nil)
(defvar nyx-modeline-selected-window nil)

(defun nyx-modeline-set-selected-window (&rest _)
  "Set selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq nyx-modeline-selected-window window))))

(add-hook 'window-configuration-change-hook #'nyx-modeline-set-selected-window)
(add-hook 'focus-in-hook #'nyx-modeline-set-selected-window)
(advice-add 'select-window :after 'nyx-modeline-set-selected-window)
(advice-add 'select-frame  :after 'nyx-modeline-set-selected-window)


;; Helpers

(defun nyx-modeline-short-buffer-name ()
  "Return only the file name if `buffer-name' is a path."
  (if buffer-file-name
      (car (last (split-string (buffer-name) "/")))
    (buffer-name)))

(defun nyx-modeline-project-relative-buffer-path ()
  "Path of current buffer relative to project root."
  (if (and (not (file-remote-p default-directory))
           (project-current))
      (replace-regexp-in-string "^.\/" ""
                                (file-relative-name default-directory (project-root (project-current))))
    (abbreviate-file-name default-directory)))

(defun nyx-modeline-shorten-directory (dir max-length)
  "Show up to `MAX-LENGTH' characters of a directory name `DIR'."
  (let* ((host-local-dir (car (reverse (split-string (abbreviate-file-name dir) ":"))))
         (path (reverse (split-string host-local-dir "/")))
         (output ""))
    (when (and path
               (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path
                (< (length output) (- max-length 4)))
      (let ((segment (car path)))

        (unless (equal "" segment)
          (unless (and (eq (length output) 0)
                       (< (length segment) (- max-length 4)))
            (setq segment (substring segment 0 1))))
        (setq output (concat segment "/" output))
        (setq path (cdr path))))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Mode line parts

(defun nyx-modeline-buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix prefer-utf-8-unix undecided-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun nyx-modeline-buffer-name ()
  "Return the formatted buffer name with appropriate faces."
  (unless (eq major-mode 'dired-mode)
    (propertize
     (if buffer-file-name (nyx-modeline-short-buffer-name) "%b")
     'face (cond ((buffer-modified-p)
                  'mode-line-filename-modified-face)
                 (nyx-modeline-active
                  (if buffer-read-only
                      'mode-line-filename-readonly-face
                    'mode-line-filename-face))))))

(defun nyx-modeline-buffer-path (max-length)
  "Format and return the buffer path truncated to MAX-LENGTH characters."
  (propertize
   (if (or buffer-file-name
           (eq major-mode 'dired-mode))
       (nyx-modeline-shorten-directory (nyx-modeline-project-relative-buffer-path) max-length) "")
   'face (if nyx-modeline-active 'mode-line-folder-face)))

(defun nyx-modeline-dedicated ()
  "Display a symbol if buffer is dedicated."
  (if (window-dedicated-p)
      (propertize "‡ " 'face (if nyx-modeline-active 'mode-line-dedicated-face))))

(defun nyx-modeline-macro-recording ()
  "Display current macro being recorded."
  (if (and nyx-modeline-active defining-kbd-macro)
      (propertize "[rec] " 'face 'mode-line-highlight)))

(defun nyx-modeline-major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (propertize (format-mode-line mode-name)
                      'face (if nyx-modeline-active 'mode-line-mode-face))
          (if (stringp mode-line-process) mode-line-process)
          ;;(if doom-ml--env-version (concat " " doom-ml--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun nyx-modeline-position ()
  "Return the cursor position with appropriate faces."
  (concat (propertize
           " %l:%c "
           'face (if (and nyx-modeline-active (>= (current-column) 81))
                     'mode-line-80col-face
                   'mode-line-position-face)) "%p"))

;;
;; Project
;;

(defun nyx-modeline-project-p ()
  "Should we display the project modeline?"
  (if (and (not (file-remote-p default-directory))
           (project-current)) t))

(defun nyx-modeline-project ()
  "Project name."
  (let ((name (when (and (nyx-modeline-project-p)
                         (or buffer-file-name (eq major-mode 'dired-mode)))
                (project-name (project-current)))))
    (when name
      (concat (propertize name 'face (if nyx-modeline-active 'mode-line-project-face))
              (propertize "/" 'face (if nyx-modeline-active 'mode-line-folder-face))))))

;;
;; Perspective
;;

(defvar-local nyx-modeline--persp-name nil)

(defun nyx-modeline-update-persp-name (&rest _)
  "Update the modeline persp name."
  (setq nyx-modeline--persp-name
        (if persp-mode
            (let* ((persp (get-frame-persp (selected-frame)))
                   (persp-name (safe-persp-name persp)))
              (unless (and (nyx-modeline-project-p)
                           (equal persp-name (project-name (project-current))))
                persp-name)))))

(defun nyx-modeline-persp-name ()
  "Displays the current perspective name if it differs from the current project."
  (when nyx-modeline--persp-name
    (propertize (concat "[" nyx-modeline--persp-name "] ")
                'face (if nyx-modeline-active 'mode-line-persp-face))))

(add-hook 'find-file-hook #'nyx-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'nyx-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'nyx-modeline-update-persp-name)
(advice-add #'select-window :after #'nyx-modeline-update-persp-name)

;;
;; Remote host
;;

(defvar-local nyx-modeline--remote-host nil)

(defun nyx-modeline-update-remote-host (&rest _)
  (setq nyx-modeline--remote-host
        (if (file-remote-p default-directory)
            (let* ((user (file-remote-p default-directory 'user))
                   (host (file-remote-p default-directory 'host))
                   (short-host (car (split-string host "\\."))))
              (if user
                  (concat user "@" short-host ":")
                (concat short-host ":"))))))

(add-hook 'find-file-hook #'nyx-modeline-update-remote-host)
(advice-add #'select-window :after #'nyx-modeline-update-remote-host)

(defun nyx-modeline-remote-host ()
  "Displays the remote user and hostname if the current buffer is remote."
  (when nyx-modeline--remote-host
    (propertize nyx-modeline--remote-host
                'face (if nyx-modeline-active 'mode-line-remote-host-face))))

;;
;; vc
;;

(defun nyx-modeline-vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend vc-mode)
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'mode-line-vcs-info-face)
                        ((memq state '(removed needs-merge needs-update
                                               conflict removed unregistered))
                         'mode-line-vcs-warning-face)
                        ('t 'mode-line-vcs-face)))))
      (if nyx-modeline-active (propertize backend 'face face) backend))))

;; Mode line
(defun nyx-modeline-format (&optional _id)
  "Return mode line format specification."
  `(:eval
    (let* ((nyx-modeline-active (eq (selected-window) nyx-modeline-selected-window))
           (width (window-total-width (selected-window)))
           (path-width (max (- width
                               (length (nyx-modeline-dedicated))
                               (length (nyx-modeline-macro-recording))
                               (length (nyx-modeline-persp-name))
                               (length (nyx-modeline-remote-host))
                               (length (nyx-modeline-project))
                               (length (nyx-modeline-buffer-name))
                               (length (nyx-modeline-buffer-encoding-abbrev))
                               (length (nyx-modeline-vc))
                               (length (nyx-modeline-major-mode))
                               (length (nyx-modeline-position))
                               5)
                            4))
           (lhs (list
                 " "
                 (nyx-modeline-dedicated)
                 (nyx-modeline-macro-recording)
                 (nyx-modeline-persp-name)
                 (nyx-modeline-remote-host)
                 (nyx-modeline-project)
                 (nyx-modeline-buffer-path path-width)
                 (nyx-modeline-buffer-name)))
           (rhs (list
                 (nyx-modeline-buffer-encoding-abbrev)
                 (nyx-modeline-vc)
                 "  "
                 (nyx-modeline-major-mode)
                 " "
                 (nyx-modeline-position)))
           (spacing (propertize
                     " "
                     'display `((space :align-to
                                       (- (+ right right-fringe right-margin)
                                          ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs spacing rhs))))

(setq-default mode-line-format (nyx-modeline-format))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(provide 'nyx-modeline)
;;; nyx-modeline.el ends here
