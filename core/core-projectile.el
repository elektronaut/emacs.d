;;; core-projectile -- Projectile config
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-hydra)
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

  (defun projectile-org-file ()
    "`org-mode' file for project."
    (if (projectile-project-p)
        (concat (if (string-match-p "Dev/anyone" (projectile-project-p))
                    "~/Dropbox/org/anyone/"
                  "~/Dropbox/org/")
                (projectile-project-name)
                ".org")))

  (defun projectile-open-org ()
    "Open `org-mode' file for project."
    (interactive)
    (if (projectile-project-p)
        (find-file (projectile-org-file))))

  (defun advice-projectile-use-rg (vcs)
    "Generate list of files using ripgrep."
    "rg --null --files")
  (advice-add 'projectile-get-ext-command :override #'advice-projectile-use-rg)

  (defun projectile-switch-persp-project (&optional arg)
    (interactive "P")
    (persp-kill-empty)
    (let* ((persps (persp-names-recent))
           (projects (projectile-relevant-known-projects))
           (targets (append (rest persps) (list (first persps)) projects)))
      (if targets
          (projectile-completing-read
           "Switch to perspective or project: " targets
           :action (lambda (target)
                     (cond ((-contains? persps target) (persp-frame-switch target))
                           ((-contains? projects target)
                            (let* ((persp-name (projectile-default-project-name target))
                                   (persp-exists (persp-with-name-exists-p persp-name))
                                   (persp (persp-add-new persp-name)))
                              (persp-frame-switch persp-name)
                              (unless persp-exists
                                (projectile-switch-project-by-name target arg))))
                           (t (user-error "Unknown target: %s" target)))))
          (user-error "There are no known projects"))))

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
    ("f" projectile-find-file)
    ("r" projectile-recentf)
    ("F" projectile-find-file-in-directory)
    ("d" projectile-find-dir)
    ("D" projectile-dired)
    ("O" projectile-open-org)

    ("<right>" projectile-next-project-buffer)
    ("<left>" projectile-previous-project-buffer)
    ("k" kill-current-buffer)
    ("b" projectile-switch-to-buffer)
    ("B" projectile-ibuffer)
    ("C-s" projectile-save-project-buffers)

    ("s" counsel-projectile-rg)
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
