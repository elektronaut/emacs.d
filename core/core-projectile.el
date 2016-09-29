;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package projectile
  :bind (("s-p"  . projectile-command-map))
  :init
  (setq projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" savefile-dir)
        projectile-cache-file
          (expand-file-name  "projectile.cache" savefile-dir))
  :config
  (add-to-list 'projectile-globally-ignored-directories "import/site")
  (projectile-global-mode t))

(provide 'core-projectile)
;;; core-projectile ends here
