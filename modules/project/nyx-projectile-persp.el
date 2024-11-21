;;; nyx-projectile-persp.el --- Projectile persp integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-projectile)
(require 'nyx-consult)
(require 'nyx-persp)

(defvar projectile-switch-persp--persp-history nil)

(defface projectile-switch-persp-perspective-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `projectile-switch-persp'."
  :group 'projectile-switch-persp)

(defvar projectile-switch-persp--source-persp
  (list :name     "Perspectives"
        :narrow   '(?p . "Perspective")
        :category 'projectile-switch-persp-perspective
        :face     'projectile-switch-persp-perspective-face
        :history  'projectile-switch-persp--persp-history
        :annotate (lambda (_persp) (format "Perspective"))
        :action   (lambda (persp) (persp-frame-switch persp))
        :items    #'persp-names-recent))

(defvar projectile-switch-persp--source-project
  (list :name     "Known Projects"
        :narrow   '(?r . "Project")
        :category 'consult-projectile-project
        :face     'consult-projectile-projects
        :history  'consult-projectile--project-history
        :annotate (lambda (dir)
                    (when consult-projectile-display-info
                      (format "Project: %s [%s]"
                              (projectile-project-name dir)
                              (projectile-project-vcs dir))))
        :action   (lambda (dir) (projectile-switch-persp-project dir))
        :items    #'projectile-relevant-known-projects))

(defun projectile-switch-persp-project (dir)
  "Switch to perspective for project in DIR."
  (let* ((persp-name (projectile-default-project-name dir))
         (persp-exists (persp-with-name-exists-p persp-name)))
    (persp-add-new persp-name)
    (persp-frame-switch persp-name)
    (unless persp-exists
      (projectile-switch-project-by-name dir))))

(defun projectile-switch-persp (&optional sources)
  "Switch to project perspective."
  (interactive)
  (when-let (buffer (consult--multi (or sources
                                        '(projectile-switch-persp--source-persp
                                          projectile-switch-persp--source-project))
                                    :prompt "Switch to: "
                                    :history 'projectile-switch-persp--persp-history
                                    :sort nil))))

(provide 'nyx-projectile-persp)
;;; nyx-projectile-persp.el ends here
