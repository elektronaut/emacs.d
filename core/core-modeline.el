;;; core-modeline.el -- Mode line
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;  Based on doom-themes by Henrik Lissner <henrik@lissner.net>
;;  https://github.com/hlissner/.emacs.d

;;; Licence: MIT

;;; Code:

(require 'core-theme)
(require 'core-ui)
(require 'core-projectile)

(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-filename-modified-face)
(make-face 'mode-line-filename-readonly-face)
(make-face 'mode-line-remote-host-face)
(make-face 'mode-line-project-face)
(make-face 'mode-line-vcs-face)
(make-face 'mode-line-vcs-info-face)
(make-face 'mode-line-vcs-warning-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(defun core-configure-modeline-faces ()
  "Configures modeline faces."
  (interactive)

  (let ((background       (face-attribute 'vertical-border :background))
        (background-faded (face-attribute 'hl-line :background))
        (foreground       (face-attribute 'default :foreground))
        (faded            (face-attribute 'font-lock-comment-face :foreground))
        (context          (face-attribute 'font-lock-keyword-face :foreground))
        (modified         (face-attribute 'warning :foreground))
        (warning          (face-attribute 'error :foreground))
        (ok               (face-attribute 'success :foreground)))

    (set-face-attribute 'mode-line nil
                        :foreground foreground :background background
                        :inverse-video nil
                        :box `(:line-width 6 :color ,background :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'mode-line-face
                        :foreground faded :background background-faded
                        :box `(:line-width 6 :color ,background-faded :style nil))
    (set-face-attribute 'mode-line-highlight nil
                        :inherit 'mode-line-face
                        :foreground warning :background nil)
    (set-face-attribute 'mode-line-read-only-face nil
                        :inherit 'mode-line-face
                        :foreground context
                        :box '(:line-width 2 :color context))
    (set-face-attribute 'mode-line-modified-face nil
                        :inherit 'mode-line-face
                        :foreground warning
                        :box '(:line-width 2 :color warning))
    (set-face-attribute 'mode-line-remote-host-face nil
                        :inherit 'mode-line-face
                        :foreground context
                        :weight 'bold)
    (set-face-attribute 'mode-line-project-face nil
                        :inherit 'mode-line-face
                        :foreground context :weight 'bold)
    (set-face-attribute 'mode-line-folder-face nil
                        :inherit 'mode-line-face
                        :foreground context)
    (set-face-attribute 'mode-line-filename-face nil
                        :inherit 'mode-line-face
                        :foreground foreground
                        :weight 'bold)
    (set-face-attribute 'mode-line-filename-modified-face nil
                        :inherit 'mode-line-face
                        :foreground modified
                        :weight 'bold)
    (set-face-attribute 'mode-line-filename-readonly-face nil
                        :inherit 'mode-line-face
                        :foreground context
                        :weight 'bold)
    (set-face-attribute 'mode-line-position-face nil
                        :inherit 'mode-line-face)
    (set-face-attribute 'mode-line-mode-face nil
                        :inherit 'mode-line-face
                        :foreground foreground)
    (set-face-attribute 'mode-line-minor-mode-face nil
                        ;;:inherit 'mode-line-mode-face
                        :height 110)
    (set-face-attribute 'mode-line-vcs-face nil
                        :inherit 'mode-line-face
                        :foreground ok)
    (set-face-attribute 'mode-line-vcs-info-face nil
                        :inherit 'mode-line-vcs-face
                        :foreground modified)
    (set-face-attribute 'mode-line-vcs-warning-face nil
                        :inherit 'mode-line-vcs-face
                        :foreground warning)
    (set-face-attribute 'mode-line-process-face nil
                        :inherit 'mode-line-face
                        :foreground ok)
    (set-face-attribute 'mode-line-80col-face nil
                        :inherit 'mode-line-position-face
                        :foreground warning)
    ;;(set-face-attribute 'rbenv-active-ruby-face nil
    ;;                    :inherit 'mode-line-face
    ;;                    :foreground nil :weight 'normal)
    ))

(core-configure-modeline-faces)

(defun load-theme--restore-modeline-faces(_ &rest _)
  "Restore modeline faces after loading theme."
  (core-configure-modeline-faces))
(advice-add 'load-theme :after #'load-theme--restore-modeline-faces)


;; Selected window

(defvar core-modeline-selected-window nil)

(defun core-modeline-set-selected-window (&rest _)
  "Set selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq core-modeline-selected-window window))))

(add-hook 'window-configuration-change-hook #'core-modeline-set-selected-window)
(add-hook 'focus-in-hook #'core-modeline-set-selected-window)
(advice-add 'select-window :after 'core-modeline-set-selected-window)
(advice-add 'select-frame  :after 'core-modeline-set-selected-window)


;; Helpers

(defun core-modeline-short-buffer-name ()
  "Return only the file name if `buffer-name' is a path."
  (if buffer-file-name
      (car (last (split-string (buffer-name) "/")))
    (buffer-name)))

(defun core-modeline-project-relative-buffer-path ()
  (if (and (not (file-remote-p default-directory))
           (projectile-project-p))
      (replace-regexp-in-string "^.\/" ""
        (car (projectile-make-relative-to-root (list default-directory))))
    (abbreviate-file-name default-directory)))

(defun core-modeline-shorten-directory (dir max-length)
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

(defun core-modeline-buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix prefer-utf-8-unix undecided-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun core-modeline-buffer-name ()
  (unless (eq major-mode 'dired-mode)
    (propertize
     (if buffer-file-name (core-modeline-short-buffer-name) "%b")
     'face (cond ((buffer-modified-p)
                  'mode-line-filename-modified-face)
                 (active
                  (if buffer-read-only
                      'mode-line-filename-readonly-face
                    'mode-line-filename-face))))))

(defun core-modeline-buffer-path (max-length)
  (propertize
   (if (or buffer-file-name
           (eq major-mode 'dired-mode))
       (core-modeline-shorten-directory (core-modeline-project-relative-buffer-path) max-length) "")
   'face (if active 'mode-line-folder-face)))

(defun core-modeline-macro-recording ()
  "Display current macro being recorded."
  (if (and active defining-kbd-macro)
      (propertize "[rec] " 'face 'mode-line-highlight)))

(defun core-modeline-major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (propertize (format-mode-line mode-name)
                      'face (if active 'mode-line-mode-face))
          (if (stringp mode-line-process) mode-line-process)
          ;;(if doom-ml--env-version (concat " " doom-ml--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun core-modeline-position ()
  (concat (propertize
           " %l:%c "
           'face (if (and active (>= (current-column) 81))
                     'mode-line-80col-face
                   'mode-line-position-face)) "%p"))

(defun core-modeline-projectile ()
  (if (and (not (file-remote-p default-directory))
           (projectile-project-p)
           (or buffer-file-name (eq major-mode 'dired-mode)))
      (concat (propertize (projectile-project-name)
                          'face (if active 'mode-line-project-face))
              (propertize "/"
                          'face (if active 'mode-line-folder-face)))))

(defun core-modeline-remote-host ()
  "Displays the remote user and hostname if the current buffer is remote."
  (if (file-remote-p default-directory)
      (let* ((user (file-remote-p default-directory 'user))
             (host (file-remote-p default-directory 'host))
             (short-host (car (split-string host "\\."))))
        (propertize (if user
                        (concat user "@" short-host ":")
                      (concat short-host ":"))
                    'face (if active 'mode-line-remote-host-face)))))

(defun core-modeline-vc ()
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
(defun core-modeline-format (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) core-modeline-selected-window))
           (width (window-total-width (selected-window)))
           (path-width (max (- width
                               (length (core-modeline-macro-recording))
                               (length (core-modeline-remote-host))
                               (length (core-modeline-projectile))
                               (length (core-modeline-buffer-name))
                               (length (core-modeline-buffer-encoding-abbrev))
                               (length (core-modeline-vc))
                               (length (core-modeline-major-mode))
                               (length (core-modeline-position))
                               5)
                            4))
           (lhs (list
                 " "
                 (core-modeline-macro-recording)
                 (core-modeline-remote-host)
                 (core-modeline-projectile)
                 (core-modeline-buffer-path path-width)
                 (core-modeline-buffer-name)))
           (rhs (list
                 (core-modeline-buffer-encoding-abbrev)
                 (core-modeline-vc)
                 "  "
                 (core-modeline-major-mode)
                 " "
                 (core-modeline-position)))
           (spacing (propertize
                     " "
                     'display `((space :align-to
                                       (- (+ right right-fringe right-margin)
                                          ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs spacing rhs))))

(setq-default mode-line-format (core-modeline-format))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(provide 'core-modeline)
;;; core-modeline.el ends here
