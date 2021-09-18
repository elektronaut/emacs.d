;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-persp)
(require 'dash)

(use-package projectile
  :after (persp-mode)
  :demand t
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-switch-persp-project)
              ("C-c p P" . projectile-switch-project)
              ("C-c p O" . projectile-open-org))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-current-project-on-switch 'move-to-end
        projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" savefile-dir)
        projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)
        projectile-switch-project-action 'projectile-dired)
  :config
  (dolist (item '("import/site"
                  "app/assets/builds"))
    (add-to-list 'projectile-globally-ignored-directories item))

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (projectile-global-mode t)

  (defun projectile-open-org ()
    "Open `org-mode' file for project."
    (interactive)
    (find-file (concat "~/Dropbox/org/anyone/"
                       (projectile-project-name)
                       ".org")))
  (defun advice-projectile-use-rg (vcs)
    "Generate list of files using ripgrep."
    "rg --null --files")
  (advice-add 'projectile-get-ext-command :override #'advice-projectile-use-rg)

  (defun projectile-switch-persp-project (&optional arg)
    (interactive "P")
    (persp-kill-empty)
    (let ((projects (projectile-relevant-known-projects)))
      (if projects
          (projectile-completing-read
           "Switch to project perspective: " projects
           :action (lambda (project)
                     (message project)
                     (if (-contains? projects project)
                         (let* ((persp-name (projectile-default-project-name project))
                                (persp-exists (persp-with-name-exists-p persp-name))
                                (persp (persp-add-new persp-name)))
                           (persp-frame-switch persp-name)
                           (unless persp-exists
                             (projectile-switch-project-by-name project arg)))
                       (user-error "Unknown project: %s" project))))
        (user-error "There are no known projects")))))


(provide 'core-projectile)
;;; core-projectile ends here
