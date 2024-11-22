;;; nyx-project-persp.el --- Project persp integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-consult)
(require 'nyx-persp)

(defvar project-persp--dir-history nil)
(defvar project-persp--persp-history nil)
(defvar project-persp--project-history nil)

(defface project-persp-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `project-persp'."
  :group 'project-persp)

(defvar project-persp--source-persp
  (list :name     "Perspectives"
        :narrow   '(?p . "Perspective")
        :category 'project-persp-perspective
        :face     'project-persp-face
        :history  'project-persp--persp-history
        :annotate (lambda (_persp) (format "Perspective"))
        :action   (lambda (persp) (persp-frame-switch persp))
        :items    #'persp-names-recent))

(defvar project-persp--source-project
  (list :name     "Known Projects"
        :narrow   '(?r . "Project")
        :category 'project-persp-project
        :face     'project-persp-face
        :history  'project-persp--project-history
        :annotate (lambda (dir)
                    (format "Project: %s"
                            (file-name-nondirectory (directory-file-name dir))))
        :action   (lambda (dir) (project-persp-project dir))
        :items    #'project-known-project-roots))

(defun project-persp-project (dir)
  "Switch to perspective for project in DIR."
  (let* ((persp-name (file-name-nondirectory (directory-file-name dir)))
         (persp-exists (persp-with-name-exists-p persp-name)))
    (persp-add-new persp-name)
    (persp-frame-switch persp-name)
    (unless persp-exists
      (project--remember-dir dir)
      ;;(project-switch-project dir)
      (dired dir))))

(defun project-persp-switch ()
  "Switch to project perspective."
  (interactive)
  (when-let (buffer (consult--multi '(project-persp--source-persp
                                      project-persp--source-project)
                                    :prompt "Switch to: "
                                    :history 'project-persp--persp-history
                                    :sort nil))))

(defun project-persp-find-and-switch ()
  "Find a directory and create/switch to its project perspective."
  (interactive)
  (let* ((dir (read-directory-name "Project to add: "))
         (project (project-current nil dir)))
    (if project
        (project-persp-project (project-root project))
      (message "Not a valid project directory: %s" dir))))

(provide 'nyx-project-persp)
;;; nyx-project-persp.el ends here
