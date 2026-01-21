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

(defvar project-persp-worktree-parents nil
  "List of parent directories that use worktree structure.
Projects that are direct subdirectories of these paths will use
\"parent/project\" naming convention.")

(defun project-persp-add-worktree-parent (dir)
  "Add DIR to `project-persp-worktree-parents'."
  (interactive "Worktree parent to add: ")
  (let ((dir (directory-file-name (expand-file-name dir))))
    (if (member dir project-persp-worktree-parents)
        (message "Already in worktree parents: %s" dir)
      (add-to-list 'project-persp-worktree-parents dir)
      (message "Added to worktree parents: %s" dir))))

(defun project-persp--git-worktree-child-p (dir)
  "Return non-nil if DIR is a git worktree child (not the main repo)."
  (let ((git-file (expand-file-name ".git" dir)))
    (and (file-exists-p git-file)
         (not (file-directory-p git-file)))))

(defun project-persp--worktree-parent-p (dir)
  "Return non-nil if DIR is a subdirectory of a configured worktree parent."
  (let ((parent-dir (file-name-directory (directory-file-name dir))))
    (cl-some (lambda (worktree-parent)
               (file-equal-p parent-dir (expand-file-name worktree-parent)))
             project-persp-worktree-parents)))

(defun project-persp--worktree-structure-p (dir)
  "Return non-nil if DIR uses worktree naming (parent/project).
True if any of:
- DIR is a git worktree child (.git is a file)
- DIR basename is \"main\" or \"master\"
- DIR parent is in `project-persp-worktree-parents'"
  (let ((basename (file-name-nondirectory (directory-file-name dir))))
    (or (project-persp--git-worktree-child-p dir)
        (member basename '("main" "master"))
        (project-persp--worktree-parent-p dir))))

(defun project-persp--name-for-dir (dir)
  "Generate perspective name for DIR.
If DIR uses worktree structure, returns \"parent/project\".
Otherwise returns just the directory name."
  (let* ((dir (directory-file-name dir))
         (basename (file-name-nondirectory dir))
         (parent-dir (file-name-directory dir))
         (parent-name (file-name-nondirectory (directory-file-name parent-dir))))
    (if (project-persp--worktree-structure-p dir)
        (format "%s/%s" parent-name basename)
      basename)))

(defun project-persp-project (dir)
  "Switch to perspective for project in DIR."
  (let* ((persp-name (project-persp--name-for-dir dir))
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
