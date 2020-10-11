;;; nyx-modeline.el -- Mode line
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;  Based on doom-themes by Henrik Lissner <henrik@lissner.net>
;;  https://github.com/hlissner/.emacs.d

;;; Licence: MIT

;;; Code:

(require 'core-projectile)
(require 'nyx-modeline-faces)

;; Selected window

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

(defvar-local nyx-modeline--project-relative-buffer-path nil)

(defun nyx-modeline-short-buffer-name ()
  "Return only the file name if `buffer-name' is a path."
  (if buffer-file-name
      (car (last (split-string (buffer-name) "/")))
    (buffer-name)))

(defun nyx-modeline-update-project-relative-buffer-path (&rest _)
  "Update the cacher project relative buffer path."
  (setq nyx-modeline--project-relative-buffer-path
        (if (and (not (file-remote-p default-directory))
                 (projectile-project-p))
            (replace-regexp-in-string "^.\/" ""
                                      (car (projectile-make-relative-to-root (list default-directory))))
          (abbreviate-file-name default-directory))))

(defun nyx-modeline-project-relative-buffer-path ()
  nyx-modeline--project-relative-buffer-path)

(add-hook 'find-file-hook #'nyx-modeline-update-project-relative-buffer-path)
(add-hook 'projectile-after-switch-project-hook #'nyx-modeline-update-project-relative-buffer-path)
(advice-add #'select-window :after #'nyx-modeline-update-project-relative-buffer-path)

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
  (unless (eq major-mode 'dired-mode)
    (propertize
     (if buffer-file-name (nyx-modeline-short-buffer-name) "%b")
     'face (cond ((buffer-modified-p)
                  'mode-line-filename-modified-face)
                 (active
                  (if buffer-read-only
                      'mode-line-filename-readonly-face
                    'mode-line-filename-face))))))

(defun nyx-modeline-buffer-path (max-length)
  (propertize
   (if (or buffer-file-name
           (eq major-mode 'dired-mode))
       (nyx-modeline-shorten-directory nyx-modeline--project-relative-buffer-path max-length) "")
   'face (if active 'mode-line-folder-face)))

(defun nyx-modeline-macro-recording ()
  "Display current macro being recorded."
  (if (and active defining-kbd-macro)
      (propertize "[rec] " 'face 'mode-line-highlight)))

(defun nyx-modeline-major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (propertize (format-mode-line mode-name)
                      'face (if active 'mode-line-mode-face))
          (if (stringp mode-line-process) mode-line-process)
          ;;(if doom-ml--env-version (concat " " doom-ml--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun nyx-modeline-position ()
  (concat (propertize
           " %l:%c "
           'face (if (and active (>= (current-column) 81))
                     'mode-line-80col-face
                   'mode-line-position-face)) "%p"))

;;
;; projectile
;;

(defvar-local nyx-modeline--projectile nil)

(defun nyx-modeline-projectile-p ()
  "Should we display the projectile modeline?"
  (if (and (not (file-remote-p default-directory))
           (projectile-project-p)) t))

(defun nyx-modeline-update-projectile (&rest _)
  "Update projectile name."
  (setq nyx-modeline--projectile
        (when (and (nyx-modeline-projectile-p)
                   (or buffer-file-name (eq major-mode 'dired-mode)))
          (projectile-project-name))))

(defun nyx-modeline-projectile ()
  "Projectile name."
  (when nyx-modeline--projectile
    (concat (propertize nyx-modeline--projectile
                        'face (if active 'mode-line-project-face))
            (propertize "/"
                        'face (if active 'mode-line-folder-face)))))

(add-hook 'find-file-hook #'nyx-modeline-update-projectile)
(add-hook 'projectile-after-switch-project-hook #'nyx-modeline-update-projectile)
(advice-add #'select-window :after #'nyx-modeline-update-projectile)

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
              (unless (and (nyx-modeline-projectile-p)
                           (equal persp-name (projectile-project-name)))
                persp-name)))))

(defun nyx-modeline-persp-name ()
  "Displays the current perspective name if it differs from the current projectile project."
  (when nyx-modeline--persp-name
    (propertize (concat "[" nyx-modeline--persp-name "] ")
                'face (if active 'mode-line-persp-face))))

(add-hook 'find-file-hook #'nyx-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'nyx-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'nyx-modeline-update-persp-name)
(advice-add #'select-window :after #'nyx-modeline-update-persp-name)

;;
;; Perspective
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
  (when nyx-modeline--remote-host
    (propertize nyx-modeline--remote-host
                'face (if active 'mode-line-remote-host-face))))

(defun nyx-modeline-remote-host2 ()
  "Displays the remote user and hostname if the current buffer is remote."
  (if (file-remote-p default-directory)
      (let* ((user (file-remote-p default-directory 'user))
             (host (file-remote-p default-directory 'host))
             (short-host (car (split-string host "\\."))))
        (propertize (if user
                        (concat user "@" short-host ":")
                      (concat short-host ":"))
                    'face (if active 'mode-line-remote-host-face)))))

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
      (if active (propertize backend 'face face) backend))))

;; Mode line
(defun nyx-modeline-format (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) nyx-modeline-selected-window))
           (width (window-total-width (selected-window)))
           (path-width (max (- width
                               (length (nyx-modeline-macro-recording))
                               (length (nyx-modeline-persp-name))
                               (length (nyx-modeline-remote-host))
                               (length (nyx-modeline-projectile))
                               (length (nyx-modeline-buffer-name))
                               (length (nyx-modeline-buffer-encoding-abbrev))
                               (length (nyx-modeline-vc))
                               (length (nyx-modeline-major-mode))
                               (length (nyx-modeline-position))
                               5)
                            4))
           (lhs (list
                 " "
                 (nyx-modeline-macro-recording)
                 (nyx-modeline-persp-name)
                 (nyx-modeline-remote-host)
                 (nyx-modeline-projectile)
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
