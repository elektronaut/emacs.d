;;; nyx-projectile-org.el --- Projectile org-mode integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'no-littering)
(require 'nyx-org-agenda)
(require 'nyx-projectile)

(defcustom projectile-org-files-file
  (no-littering-expand-etc-file-name "project-org-files.el")
  "Name and location of the Projectile's known org projects file."
  :group 'projectile-org
  :type 'string)

(defcustom projectile-org-files '()
  "Alist mapping project names to their org files."
  :type '(alist :key-type string :value-type file)
  :group 'projectile)

(defun projectile-org-file ()
  "`org-mode' file for project."
  (if (projectile-project-p)
      (let* ((basename (concat (projectile-project-name) ".org"))
             (files (seq-filter (lambda (f) (string-match-p basename f))
                                (org-agenda-files))))
        (or (cdr (assoc (projectile-project-name) projectile-org-files))
            (car files)
            (concat org-directory "/" basename)))))

(defun projectile-open-org ()
  "Open `org-mode' file for project."
  (interactive)
  (if (projectile-project-p)
      (find-file (projectile-org-file))))

(defun projectile-org-files-save ()
  "Save =projectile-org-files' to disk."
  (interactive)
  (with-temp-buffer
    (print projectile-org-files (current-buffer))
    (write-file projectile-org-files-file)))

(defun projectile-org-files-load ()
  "Load =projectile-org-files' from disk."
  (interactive)
  (when (file-exists-p projectile-org-files-file)
    (with-temp-buffer
      (insert-file-contents projectile-org-files-file)
      (setq projectile-org-files (read (current-buffer))))))

(defun projectile-org-set-file (org-file)
  "Set ORG-FILE for the current project."
  (interactive (list (read-file-name "Set org file: " (concat org-directory "/"))))
  (when (projectile-project-p)
    (setq projectile-org-files
          (cons (cons (projectile-project-name) org-file)
                (assoc-delete-all (projectile-project-name) projectile-org-files)))
    (projectile-org-files-save)))

(projectile-org-files-load)

(provide 'nyx-projectile-org)
;;; nyx-projectile-org.el ends here