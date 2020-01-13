;;; core-perspective.el --- Perspective
;;; Commentary:
;;; Code:

(use-package perspective
  :ensure t
  :bind (:map persp-mode-map
              ("C-x x C" . persp-kill-other))
  :config
  (persp-mode)
  (defun persp-kill-other ()
    "Kill other perspectives."
    (interactive)
    (mapc 'persp-kill (remove (persp-name (persp-curr))
                              (remove "org" (persp-names))))))

(use-package persp-projectile
  :ensure t)

(provide 'core-perspective)
;;; core-perspective.el ends here
