;;; core-perspective.el --- Perspective
;;; Commentary:
;;; Code:

(use-package perspective
  :ensure t
  :config
  (persp-mode))

(use-package persp-projectile
  :ensure t)

(provide 'core-perspective)
;;; core-perspective.el ends here
