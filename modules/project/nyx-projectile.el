;;; nyx-projectile.el --- Projectile -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :demand t
  :commands insert-project-name
  :functions projectile-project-name projectile-project-p advice-projectile-use-rg
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-switch-persp)
              ;; ("C-c p P" . projectile-switch-project)
              ("C-c p C-c" . projectile-bin-dev)
              ("C-c p O" . projectile-open-org))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-current-project-on-switch 'move-to-end
        projectile-completion-system 'default
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-switch-project-action 'projectile-dired)
  :config
  (dolist (item '("import/site"
                  "app/assets/builds"))
    (add-to-list 'projectile-globally-ignored-directories item))

  (define-advice projectile-project-root (:around (orig-fun &rest args) ignore-remote)
    "Ignore `projectile-project-root` if in a remote directory."
    (if (file-remote-p default-directory)
        nil
      (apply orig-fun args)))

  (projectile-mode t)

  (defun insert-project-name ()
    "Insert current project name."
    (interactive)
    (insert (projectile-project-name)))

  (defun projectile-bin-dev ()
    "Run bin/dev in a compilation buffer."
    (interactive)
    (when-let* ((project (projectile-project-p))
                (path (concat project "bin/dev"))
                (buffer-name (concat "*bin/dev " project "*"))
                (default-directory project))
      (if (file-exists-p path)
          (compilation-start path
                             'compilation-mode
                             (lambda (_arg) buffer-name))
        (error "%s does not exist!" path))))

  (defun advice-projectile-use-rg (_)
    "Generate list of files using ripgrep."
    "rg --null --files")

  (advice-add 'projectile-get-ext-command :override #'advice-projectile-use-rg))

(provide 'nyx-projectile)
;;; nyx-projectile.el ends here
