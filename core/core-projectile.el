;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-perspective)

(use-package projectile
  :after (persp-mode)
  :demand t
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-switch-persp-project)
              ("C-c p P" . projectile-switch-project)
              ("C-c p O" . projectile-open-org))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" savefile-dir)
        projectile-cache-file
        (expand-file-name  "projectile.cache" savefile-dir))
  :config
  (add-to-list 'projectile-globally-ignored-directories "import/site")

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (projectile-global-mode t)

  (defun projectile-open-org ()
    "Open `org-mode' file for project."
    (interactive)
    (find-file (concat "~/Dropbox/org/anyone/"
                       (projectile-project-name)
                       ".org")))

  ;; Adapted from persp-mode-projectile-bridge
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el

  (defvar projectile-persp-hook-before-switch-selected-window-buffer nil)
  (defvar projectile-persp-enable-switch nil)

  (defun projectile-persp-add-new-persp (name)
    (let ((persp (persp-get-by-name name *persp-hash* :nil)))
      (if (eq :nil persp)
          (prog1
              (setq persp (persp-add-new name))
            (when persp
              (set-persp-parameter 'dont-save-to-file t persp)
              (persp-add-buffer (projectile-project-buffers)
                                persp nil nil)))
        persp)))

  (defun projectile-persp-find-perspective-for-buffer (b)
    (when (buffer-live-p b)
      (with-current-buffer b
        (when (and (buffer-file-name b)
                   (projectile-project-p))
          (let ((persp (projectile-persp-add-new-persp
                        (projectile-project-name))))
            (when persp
              (persp-add-buffer b persp nil nil)
              persp))))))

  (defun projectile-persp-hook-before-switch (&rest _args)
    (let ((win (if (minibuffer-window-active-p (selected-window))
                   (minibuffer-selected-window)
                 (selected-window))))
      (when (window-live-p win)
        (setq projectile-persp-hook-before-switch-selected-window-buffer
              (window-buffer win)))))

  (add-hook 'projectile-before-switch-project-hook
            #'projectile-persp-hook-before-switch)

  (defun projectile-persp-hook-switch (&rest _args)
    (when projectile-persp-enable-switch
      (let ((persp
             (projectile-persp-find-perspective-for-buffer
              (current-buffer))))
        (when persp
          (when (buffer-live-p
                 projectile-persp-hook-before-switch-selected-window-buffer)
            (let ((win (selected-window)))
              (unless (eq (window-buffer win)
                          projectile-persp-hook-before-switch-selected-window-buffer)
                (set-window-buffer
                 win projectile-persp-hook-before-switch-selected-window-buffer)
                (setq projectile-persp-hook-before-switch-selected-window-buffer nil))))
          (persp-frame-switch (persp-name persp))))))

  (add-hook 'projectile-after-switch-project-hook
            #'projectile-persp-hook-switch)

  (defun projectile-switch-persp-project (&optional arg)
    (interactive "P")
    (persp-kill-empty)
    (setq projectile-persp-enable-switch t)
    (let ((projects (projectile-relevant-known-projects)))
      (if projects
          (projectile-completing-read
           "Switch to project perspective: " projects
           :action (lambda (project)
                     (let* ((persp-name (projectile-default-project-name project))
                            (persp-exists (persp-with-name-exists-p persp-name))
                            (persp (persp-add-new persp-name)))
                       (if persp-exists
                           (persp-switch persp-name)
                         (projectile-switch-project-by-name project arg))
                       (setq projectile-persp-enable-switch nil))))
        (user-error "There are no known projects")))))

(provide 'core-projectile)
;;; core-projectile ends here
