;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-completion)
(require 'core-hydra)
(require 'core-persp)
(require 'dash)

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
        :annotate (lambda (persp) (format "Perspective"))
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

(use-package projectile
  :after (persp-mode)
  :demand t
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
                             (lambda (arg) buffer-name))
        (error "%s does not exist!" path))))

  ;; (defun projectile-org-file ()
  ;;   "`org-mode' file for project."
  ;;   (if (projectile-project-p)
  ;;       (concat (if (string-match-p "Dev/anyone" (projectile-project-p))
  ;;                   "~/Library/CloudStorage/Dropbox/org/anyone/"
  ;;                 "~/Library/CloudStorage/Dropbox/org/")
  ;;               (projectile-project-name)
  ;;               ".org")))

  (defun projectile-org-file ()
    "`org-mode' file for project."
    (if (projectile-project-p)
        (let* ((basename (concat (projectile-project-name) ".org"))
               (files (seq-filter (lambda (f) (string-match-p basename f))
                                  (org-agenda-files))))
          (or (car files)
              (concat org-directory "/" basename)))))

  (defun projectile-open-org ()
    "Open `org-mode' file for project."
    (interactive)
    (if (projectile-project-p)
        (find-file (projectile-org-file))))

  (defun advice-projectile-use-rg (vcs)
    "Generate list of files using ripgrep."
    "rg --null --files")
  (advice-add 'projectile-get-ext-command :override #'advice-projectile-use-rg)


  ;; Perspective switcher

  (defun projectile-switch-persp-project (dir)
    "Switch to perspective for projectile project."
    (let* ((persp-name (projectile-default-project-name dir))
           (persp-exists (persp-with-name-exists-p persp-name))
           (persp (persp-add-new persp-name)))
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


  (defhydra hydra-projectile (:hint nil)
    "
  Projectile: %(projectile-project-name)

  Find^^             Buffer^^                Search^^          Projects^^
  ^^^^^^^^^^------------------------------------------------------------------------
  _f_: file          _→_: next               _s_: rg (quick)   _p_: switch perspective
  _r_: recent file   _←_: previous           _S_: rg           _P_: switch
  _F_: file in dir   _k_: kill               ^^                _A_: add
  _d_: dir           _b_: switch to buffer   ^^                _x_: remove
  _D_: root dir      _B_: ibuffer            ^^                _X_: cleanup
  _O_: org file    _C-s_: save all           ^^

  "
    ("f" consult-projectile-find-file)
    ("r" consult-projectile-recentf)
    ("F" projectile-find-file-in-directory)
    ("d" consult-projectile-find-dir)
    ("D" projectile-dired)
    ("O" projectile-open-org)

    ("<right>" projectile-next-project-buffer)
    ("<left>" projectile-previous-project-buffer)
    ("k" kill-current-buffer)
    ("b" consult-project-buffer)
    ("B" projectile-ibuffer)
    ("C-s" projectile-save-project-buffers)

    ("s" consult-ripgrep)
    ("S" rg-project)

    ("p" projectile-switch-persp-project)
    ("P" projectile-switch-project)
    ("A" projectile-add-known-project)
    ("x" projectile-remove-known-project)
    ("X" projectile-cleanup-known-projects)
    ("q" nil "quit"))

  (define-key projectile-mode-map (kbd "C-c P") #'hydra-projectile/body))

(provide 'core-projectile)
;;; core-projectile ends here
