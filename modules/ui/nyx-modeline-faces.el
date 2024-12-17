;;; nyx-modeline-faces.el --- Mode line faces -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-theme)
(require 'nyx-ui)

(make-face 'mode-line-dedicated-face)
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-filename-modified-face)
(make-face 'mode-line-filename-readonly-face)
(make-face 'mode-line-remote-host-face)
(make-face 'mode-line-persp-face)
(make-face 'mode-line-project-face)
(make-face 'mode-line-vcs-face)
(make-face 'mode-line-vcs-info-face)
(make-face 'mode-line-vcs-warning-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(defun nyx-configure-modeline-faces ()
  "Configures modeline faces."
  (interactive)

  (let ((background       (face-attribute 'vertical-border :background))
        (background-faded (face-attribute 'hl-line :background))
        (foreground       (face-attribute 'default :foreground))
        (faded            (face-attribute 'font-lock-comment-face :foreground))
        (context          (face-attribute 'font-lock-keyword-face :foreground))
        (modified         (face-attribute 'font-lock-string-face :foreground))
        (muted            (face-attribute 'outline-1 :foreground))
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
                        :foreground warning :background 'unspecified)
    (set-face-attribute 'mode-line-dedicated-face nil
                        :inherit 'mode-line-face
                        :foreground muted)
    (set-face-attribute 'mode-line-read-only-face nil
                        :inherit 'mode-line-face
                        :foreground context
                        :box `(:line-width 2 :color ,context))
    (set-face-attribute 'mode-line-modified-face nil
                        :inherit 'mode-line-face
                        :foreground warning
                        :box `(:line-width 2 :color ,warning))
    (set-face-attribute 'mode-line-remote-host-face nil
                        :inherit 'mode-line-face
                        :foreground context
                        :weight 'bold)
    (set-face-attribute 'mode-line-persp-face nil
                        :inherit 'mode-line-face
                        :foreground modified)
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

(nyx-configure-modeline-faces)

(defun load-theme--restore-modeline-faces(_ &rest _)
  "Restore modeline faces after loading theme."
  (nyx-configure-modeline-faces))
(advice-add 'load-theme :after #'load-theme--restore-modeline-faces)

(provide 'nyx-modeline-faces)
;;; nyx-modeline-faces.el ends here
