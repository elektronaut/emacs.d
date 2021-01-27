;;; core-theme.el --- Theme
;;; Commentary:
;;; Code:

(require 'org)

(defun load-theme--disable-old-theme(theme &rest _)
  "Disable current THEME before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-horizon t)
  ;;(load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

;;(load-theme 'atom-one-dark)

(set-mouse-color "white")

(defun core-configure-theme-overrides ()
  "Set all org faces to the same height and background."
  (let ((height     (face-attribute 'default :height))
        (background (face-attribute 'default :background)))
    (dolist (face '(outline-1
                    org-level-1
                    org-level-2))
      (set-face-attribute face nil :weight 'normal
                          :height 1.0 :background background))
    (dolist (face '(org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil
                          :weight 'normal :height 1.0 :background background))
    (dolist (face '(org-agenda-structure))
      (set-face-attribute face nil :weight 'bold :height height))
    (dolist (face '(org-agenda-date
                    org-agenda-date-today
                    org-agenda-date-weekend))
      (set-face-attribute face nil :weight 'bold :height height))))

(core-configure-theme-overrides)

(defun load-theme--restore-theme-overrides(_ &rest __)
  "Restore theme overrides after loading theme."
  (core-configure-theme-overrides))
(advice-add 'load-theme :after #'load-theme--restore-theme-overrides)

;; Minibuffer background
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;             (add-to-list 'face-remapping-alist
;;                          '(default
;;                             (:background (face-attribute
;;                                           'mode-line :background))))))

(provide 'core-theme)
;;; core-theme.el ends here
