;;; nyx-project-persp.el --- Project persp integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nyx-consult)
(require 'nyx-persp)
(require 'nyx-project)

(defvar project-persp--dir-history nil)
(defvar project-persp--persp-history nil)
(defvar project-persp--project-history nil)
(defvar project-persp--worktree-history nil)

(defface project-persp-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `project-persp'."
  :group 'project-persp)

(defun project-persp-visit (name)
  "Switch to perspective NAME, saying so when its worktree is gone.
Switching still happens: a buffer in the perspective may hold unsaved
changes, and that is the only copy left."
  (persp-frame-switch name)
  (when (project-worktree-persp-dead-p name)
    (message "%s: worktree no longer exists (%s to clear it)"
             name (substitute-command-keys "\\[project-worktree-reap]"))))

(defvar project-persp--source-persp
  (list :name     "Perspectives"
        :narrow   '(?p . "Perspective")
        :category 'project-persp-perspective
        :face     'project-persp-face
        :history  'project-persp--persp-history
        :annotate (lambda (name)
                    (if-let* ((root (project-worktree-persp-root name)))
                        (cond
                         ((not (file-directory-p root)) "worktree deleted")
                         ((project-worktree-at root)
                          (or (project-worktree-branch (project-worktree-at root))
                              "detached"))
                         (t (abbreviate-file-name root)))
                      "Perspective"))
        :action   #'project-persp-visit
        :items    #'persp-names-recent))

(defvar project-persp--project-roots (make-hash-table :test 'equal)
  "Maps a candidate label to the project root it stands for.")

(defun project-persp--disambiguate (root)
  "Return the parent directory name of ROOT, for telling like-named projects apart."
  (file-name-nondirectory
   (directory-file-name (file-name-directory (directory-file-name root)))))

(defun project-persp--project-items ()
  "Return display names for all known projects, remembering their roots."
  (when-let* ((pred (alist-get 'prompt project-prune-zombie-projects))
              (inhibit-message t))
    (project--delete-zombie-projects pred))
  (clrhash project-persp--project-roots)
  (let* ((roots (project-known-project-roots))
         (names (mapcar #'project-display-name roots))
         (counts (make-hash-table :test 'equal)))
    (dolist (name names)
      (puthash name (1+ (gethash name counts 0)) counts))
    (seq-mapn (lambda (root name)
                (let ((label (if (> (gethash name counts 0) 1)
                                 (format "%s (%s)" name (project-persp--disambiguate root))
                               name)))
                  (puthash label root project-persp--project-roots)
                  label))
              roots names)))

(defvar project-persp--source-project
  (list :name     "Known Projects"
        :narrow   '(?r . "Project")
        :category 'project-persp-project
        :face     'project-persp-face
        :history  'project-persp--project-history
        :annotate (lambda (label)
                    (when-let* ((root (gethash label project-persp--project-roots)))
                      (abbreviate-file-name root)))
        :action   (lambda (label)
                    (when-let* ((root (gethash label project-persp--project-roots)))
                      (project-persp-project root)))
        :items    #'project-persp--project-items))

(defvar project-persp--worktree-roots (make-hash-table :test 'equal)
  "Maps a worktree candidate label to the worktree root it stands for.")

(defun project-persp--worktree-items ()
  "Return display names for the worktrees of every known repository.
Only repositories with more than one worktree contribute."
  (clrhash project-persp--worktree-roots)
  (when-let* ((current (project-worktree-current)))
    (project-worktree-forget (project-worktree-repo-root current)))
  (project-worktree-warm (project-known-project-roots))
  (let (labels)
    (maphash (lambda (_repo-root worktrees)
               (when (cdr worktrees)
                 (dolist (worktree worktrees)
                   (let ((label (project-worktree-label worktree)))
                     (puthash label (project-worktree-root worktree)
                              project-persp--worktree-roots)
                     (push label labels)))))
             project-worktree--cache)
    (sort labels #'string<)))

(defvar project-persp--source-worktree
  (list :name     "Worktrees"
        :narrow   '(?w . "Worktree")
        :category 'project-persp-worktree
        :face     'project-persp-face
        :history  'project-persp--worktree-history
        :hidden   t
        :annotate (lambda (label)
                    (when-let* ((root (gethash label project-persp--worktree-roots)))
                      (abbreviate-file-name root)))
        :action   (lambda (label)
                    (when-let* ((root (gethash label project-persp--worktree-roots)))
                      (project-persp-project root)))
        :items    #'project-persp--worktree-items))

(defun project-persp-project (dir)
  "Switch to perspective for project in DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (persp-name (project-display-name dir))
         (persp-exists (persp-with-name-exists-p persp-name)))
    (persp-add-new persp-name)
    (persp-frame-switch persp-name)
    (set-persp-parameter 'project-worktree-root (directory-file-name dir)
                         (persp-get-by-name persp-name))
    (unless persp-exists
      (project--remember-dir dir)
      ;;(project-switch-project dir)
      (dired dir))))

(defun project-worktree-follow-target (directory)
  "Return the root to follow into for DIRECTORY, or nil to stay put.
Only another worktree of the repository we are already in qualifies:
a sibling worktree, never a submodule, another repository, or a
directory inside the current worktree."
  (when-let* ((current (project-worktree-current)))
    (project-worktree-forget (project-worktree-repo-root current))
    (when-let* ((target (project-worktree-at directory)))
      (and (equal (project-worktree-repo-root target)
                  (project-worktree-repo-root current))
           (not (equal (project-worktree-root target)
                       (project-worktree-root current)))
           (project-worktree-root target)))))

(defun project-worktree--follow-magit (orig directory &rest args)
  "Switch perspective when magit sends us to another worktree at DIRECTORY.
ORIG runs with the directory we started in, because it decides between
Dired and a status buffer by comparing that against DIRECTORY."
  (let ((origin default-directory)
        (root (project-worktree-follow-target directory)))
    (when root
      (project-persp-project root))
    (let ((default-directory origin))
      (apply orig directory args))))

(with-eval-after-load 'magit
  (advice-add 'magit-diff-visit-directory :around #'project-worktree--follow-magit))

(defun project-worktree-switch ()
  "Switch to a worktree of the current repository."
  (interactive)
  (let* ((current (or (project-worktree-current)
                      (user-error "Not inside a git repository")))
         (target (project-worktree--read
                  current (format "Worktree in %s: "
                                  (project-worktree-repo-name current)))))
    (project-persp-project (project-worktree-root target))))

(defun project-worktree-persp-root (name)
  "Return the worktree root perspective NAME was created for, if any."
  (when-let* ((persp (persp-get-by-name name)))
    (persp-parameter 'project-worktree-root persp)))

(defun project-worktree-persp-dead-p (name)
  "Return non-nil if perspective NAME belongs to a worktree that is gone."
  (when-let* ((root (project-worktree-persp-root name)))
    (not (file-directory-p root))))

(defun project-worktree-reap (&optional silent)
  "Kill perspectives whose worktree no longer exists on disk."
  (interactive)
  (let ((dead (seq-filter #'project-worktree-persp-dead-p (persp-names))))
    (when (member (safe-persp-name (get-current-persp)) dead)
      (persp-prev))
    (dolist (name dead)
      (persp-kill name))
    (project-worktree-refresh)
    (unless silent
      (message (if dead
                   (format "Reaped %d perspective(s): %s"
                           (length dead) (string-join dead ", "))
                 "No perspectives to reap")))
    dead))

(defun project-worktree--reap-magit (&rest _)
  "Reap perspectives after magit deletes a worktree."
  (project-worktree-reap t))

(with-eval-after-load 'magit
  (advice-add 'magit-worktree-delete :after #'project-worktree--reap-magit))

(defun project-worktree--fresh-list (worktree)
  "Return the worktrees of WORKTREE's repository, freshly read."
  (let ((repo-root (project-worktree-repo-root worktree)))
    (project-worktree-forget repo-root)
    (project-worktree-list repo-root)))

(defun project-worktree--siblings (worktree)
  "Return the other worktrees of WORKTREE's repository, freshly read."
  (seq-remove (lambda (other)
                (equal (project-worktree-root other) (project-worktree-root worktree)))
              (project-worktree--fresh-list worktree)))

(defun project-worktree--read (worktree prompt)
  "Read a worktree of WORKTREE's repository using PROMPT.
WORKTREE itself is offered too, marked as the one we are in, so the
prompt looks the same whatever the number of worktrees."
  (let* ((root (project-worktree-root worktree))
         (table (mapcar (lambda (other) (cons (project-worktree-label other) other))
                        (project-worktree--fresh-list worktree)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (label)
                  (when (equal (project-worktree-root (cdr (assoc label table))) root)
                    (propertize " current" 'face 'completions-annotations))))))
    (cdr (assoc (completing-read prompt table nil t) table))))

(defun project-worktree--read-sibling (worktree prompt)
  "Read one of WORKTREE's siblings using PROMPT, or return the only one."
  (let ((others (project-worktree--siblings worktree)))
    (cond
     ((null others)
      (user-error "%s has no other worktrees" (project-worktree-repo-name worktree)))
     ((null (cdr others)) (car others))
     (t (let ((table (mapcar (lambda (other)
                               (cons (project-worktree-label other) other))
                             others)))
          (cdr (assoc (completing-read prompt table nil t) table)))))))

(defun project-worktree-perspectives ()
  "Return live perspectives for the current repository's worktrees.
Ordered as the switcher orders them: the main worktree first, then the
rest by name."
  (when-let* ((current (project-worktree-current))
              (names (persp-names)))
    (seq-filter
     (lambda (name) (member name names))
     (mapcar #'project-worktree-persp-name
             (sort (project-worktree-list (project-worktree-repo-root current))
                   (lambda (a b)
                     (cond ((project-worktree-main a) t)
                           ((project-worktree-main b) nil)
                           (t (string< (project-worktree-short-name a)
                                       (project-worktree-short-name b))))))))))

(defun project-worktree--cycle (delta)
  "Move DELTA places through the current repository's worktree perspectives."
  (let* ((current (or (project-worktree-current)
                      (user-error "Not inside a git repository")))
         (perspectives (project-worktree-perspectives)))
    (if (null (cdr perspectives))
        (message "No other worktree perspectives open for %s"
                 (project-worktree-repo-name current))
      (let ((index (or (seq-position perspectives
                                     (safe-persp-name (get-current-persp)))
                       0)))
        (persp-frame-switch
         (nth (mod (+ index delta) (length perspectives)) perspectives))))))

(defun project-worktree-next ()
  "Switch to the next worktree perspective of this repository."
  (interactive)
  (project-worktree--cycle 1))

(defun project-worktree-previous ()
  "Switch to the previous worktree perspective of this repository."
  (interactive)
  (project-worktree--cycle -1))

(defun project-worktree-visit-counterpart ()
  "Visit the current file or directory in another worktree of this repository."
  (interactive)
  (let* ((current (or (project-worktree-current)
                      (user-error "Not inside a git repository")))
         (here (or buffer-file-name
                   (and (derived-mode-p 'dired-mode) (expand-file-name default-directory))
                   (user-error "Buffer is not visiting a file")))
         (relative (file-relative-name
                    here (file-name-as-directory (project-worktree-root current)))))
    (when (string-prefix-p "../" relative)
      (user-error "%s is outside %s" here (project-worktree-persp-name current)))
    (let* ((target (project-worktree--read-sibling
                    current (format "Open %s in: " relative)))
           (file (expand-file-name
                  relative (file-name-as-directory (project-worktree-root target)))))
      (if (file-exists-p file)
          (find-file file)
        (user-error "%s does not exist in %s" relative
                    (project-worktree-persp-name target))))))

(defun project-persp-switch ()
  "Switch to project perspective."
  (interactive)
  (when-let (buffer (consult--multi '(project-persp--source-persp
                                      project-persp--source-project
                                      project-persp--source-worktree)
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
