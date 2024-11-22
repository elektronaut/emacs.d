;;; nyx-project-org.el --- Project org-mode integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'no-littering)
(require 'project)
(require 'nyx-org-agenda)

(defcustom project-org-files-file
  (no-littering-expand-var-file-name "project-org-files.el")
  "Name and location of the projects's known org file."
  :group 'project-org
  :type 'string)

(defcustom project-org-files '()
  "Alist mapping project names to their org files."
  :type '(alist :key-type string :value-type file)
  :group 'project-org)

(defun project-org-file ()
  "`org-mode' file for project."
  (if (project-current)
      (let* ((basename (concat (project-name (project-current)) ".org"))
             (files (seq-filter (lambda (f) (string-match-p basename f))
                                (org-agenda-files))))
        (or (cdr (assoc (project-name (project-current)) project-org-files))
            (car files)
            (concat org-directory "/" basename)))))

(defun project-org-open ()
  "Open `org-mode' file for project."
  (interactive)
  (if (project-current)
      (find-file (project-org-file))))

(defun project-org-files-save ()
  "Save =project-org-files' to disk."
  (interactive)
  (with-temp-buffer
    (print project-org-files (current-buffer))
    (write-file project-org-files-file)))

(defun project-org-files-load ()
  "Load =project-org-files' from disk."
  (interactive)
  (when (file-exists-p project-org-files-file)
    (with-temp-buffer
      (insert-file-contents project-org-files-file)
      (setq project-org-files (read (current-buffer))))))

(defun project-org-set-file (org-file)
  "Set ORG-FILE for the current project."
  (interactive (list (read-file-name "Set org file: " (concat org-directory "/"))))
  (when (project-current)
    (setq project-org-files
          (cons (cons (project-name (project-current)) org-file)
                (assoc-delete-all (project-name (project-current)) project-org-files)))
    (project-org-files-save)))

(project-org-files-load)

(provide 'nyx-project-org)
;;; nyx-project-org.el ends here
