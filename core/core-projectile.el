;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package projectile
  :bind (("s-p"  . projectile-command-map))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" savefile-dir)
        projectile-cache-file
          (expand-file-name  "projectile.cache" savefile-dir))
  :config
  (add-to-list 'projectile-globally-ignored-directories "import/site")
  ;; Disable projectile on remote
  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
                     (list
                      (buffer-file-name)
                      list-buffers-directory
                      default-directory
                      dired-directory))
      ad-do-it))
  (projectile-global-mode t))

(provide 'core-projectile)
;;; core-projectile ends here
