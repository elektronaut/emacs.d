;;; core-theme.el --- Theme
;;; Commentary:
;;; Code:

(require 'org)

(defun load-theme--disable-old-theme(theme &rest _)
  "Disable current THEME before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

(setq-default custom-theme-directory "~/.emacs.d/themes/")

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-ventura t)

  ;; (load-theme 'doom-1337 t)
  ;; (load-theme 'doom-feather-dark t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-henna t)
  ;; (load-theme 'doom-horizon t)
  ;; (load-theme 'doom-material-dark t)
  ;; (load-theme 'doom-molokai t)
  ;; (load-theme 'doom-monokai-spectrum t)
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-snazzy t)
  ;; (load-theme 'doom-sourcerer t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-wilmersdorf t)

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

(defun reload-theme ()
  "Reload custom theme."
  (interactive)
  (load-theme (car custom-enabled-themes) t))


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
