;;; module-org -- Org mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-hydra)
(require 'core-secrets)
(require 'org)
(require 'org-agenda)

(defun skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (has-next ))
        (save-excursion
          (forward-line 1)
          (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
            (unless (member "WAITING" (org-get-tags-at))
              (setq has-next t))))
        (if has-next
            next-headline
          nil)) ; a stuck project, has subtasks but no next task
      next-headline)))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org-archive\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory "~/Dropbox/org"
      org-agenda-compact-blocks nil
      org-agenda-files '("~/Dropbox/org"
                         "~/Dropbox/org/anyone")
      org-agenda-persistent-filter t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-span 14
      org-archive-location "%s-archive::datetree/"
      org-cycle-separator-lines 2
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-ellipsis nil
      org-global-properties (quote (("Effort_ALL" .
                                     "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit")))
      org-hide-leading-stars nil
      org-log-done t
      org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")
      org-outline-path-complete-in-steps nil
      org-tags-exclude-from-inheritance '("project")
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-replace-disputed-keys t
      org-reverse-note-order t
      org-src-fontify-natively t
      org-startup-indented t
      org-id-link-to-org-use-id t)

(setq org-agenda-custom-commands
      '(("n" "Next actions"
         ((agenda "" ((org-agenda-span 7)))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High priority")))
          (tags-todo "+inbox"  ((org-agenda-overriding-header "Inbox")))
          (todo "NEXT"  ((org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'scheduled 'deadline
                                                     'regexp "\\=.*\\[#A\\]"))
                         (org-agenda-overriding-header "Next actions")))
          (tags "project/-DONE-MAYBE-DELEGATED-CANCELLED"
                ((org-agenda-overriding-header "Active projects")))
          (todo "WAITING"  ((org-agenda-overriding-header "Waiting")))
          (stuck ""  ((org-agenda-overriding-header "Stuck projects")))))
        ("o" "Overview"
         ((agenda "" ((org-agenda-span 14)))
          (stuck "" ((org-agenda-overriding-header "Stuck projects")))
          (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
          (todo "TODO" ((org-agenda-overriding-header "Actionable tasks")))
          (todo "MAYBE" ((org-agenda-overriding-header "Someday/maybe")))
          (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))))))

(setq org-stuck-projects
      '("+project/-DONE-MAYBE-DELEGATED-CANCELLED" ("NEXT" "WAITING") nil ""))

(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING" "MAYBE" "|"
                  "DONE(d)" "DELEGATED" "CANCELLED")))

(setq org-todo-keyword-faces
      (quote (("NEXT"    :foreground "#98be65" :weight bold)
              ("TODO"    :foreground "#98be65" :weight bold)
              ("WAITING" :foreground "#51afef" :weight bold)
              ("MAYBE"   :foreground "#51afef" :weight bold))))

(setq org-capture-templates
      '(("t" "Task" entry (file+olp "~/Dropbox/org/inbox.org" "Tasks")
	 "* TODO %?\n%U\n%i" :prepend t)
        ("n" "Note" entry (file+olp "~/Dropbox/org/inbox.org" "Notes")
	 "* %? %U\n%i" :prepend t)
        ("s" "Source note" entry (file+olp "~/Dropbox/org/inbox.org" "Notes")
	 "* %? %U\n#+BEGIN_SRC\n%i\n#+END_SRC\nFrom: %a" :prepend t)
        ("j" "Journal entry" entry (file+datetree "~/Dropbox/org/journal.org")
	 "* %?\n%i")
        ("p" "Project" entry (file+olp "~/Dropbox/org/inbox.org" "Projects")
	 "* %? :project:\n** Tasks [0/0]\n" :prepend t)))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand"  . ?e)
                            ("@office"  . ?o)
                            ("@home"    . ?h)
                            (:endgroup)
                            ("project"  . ?p)
                            ("meeting"  . ?m)
                            ("maybe"    . ?M)
                            ("note"     . ?n)
                            ("tasks"    . ?t)
                            ("flagged"  . ??))))

(with-eval-after-load 'org
  ;; Prevent Org from overriding the bindings for windmove.
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<up>") nil)
  (define-key org-agenda-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<left>") nil)
  (define-key org-agenda-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "C-c o") #'hydra-org/body)
  (define-key org-mode-map (kbd "C-c :") #'org-time-stamp-inactive)

  ;; Add replacements for the some of keybindings we just removed. It
  ;; looks like Org already binds C-up and C-down separately from M-{
  ;; and M-}, so we can't use those. Users will just have to make do
  ;; with C-c <up> and C-c <down> for now.
  ;;
  ;; Now for Org Agenda on the other hand, we could use C-up and
  ;; C-down because M-{ and M-} are bound to the same commands. But I
  ;; think it's best to take the same approach as before, for
  ;; consistency.
  (define-key org-mode-map (kbd "C-<left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "C-<right>") #'org-shiftright)
  (define-key org-agenda-mode-map (kbd "C-<left>") #'org-agenda-do-date-earlier)
  (define-key org-agenda-mode-map (kbd "C-<right>") #'org-agenda-do-date-later))

(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

(use-package ox-clip)

;; (use-package org-journal
;;   :init
;;   (setq org-journal-dir "~/Dropbox/org/journal"))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-dir (expand-file-name "org-gcal/" savefile-dir)
;;         org-gcal-client-id (config-secret 'gcal-client-id)
;; 	org-gcal-client-secret (config-secret 'gcal-client-secret)
;; 	org-gcal-file-alist '(("inge@anyone.no" .  "~/Dropbox/org/gcal-anyone.org")))
;;   (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
;;   :init
;;   (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;   (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))


;; From https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
(defun org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)

;; Open zpl: links
(defun org-zpl-open (path)
  "Open PATH in Zeplin."
  (start-process-shell-command
   "open-org-zpl-process" nil
   (concat "/usr/bin/open " (shell-quote-argument (concat "zpl:" path)))))

(org-link-set-parameters "zpl" :follow #'org-zpl-open)

(defhydra hydra-org (org-mode-map "C-c o" :hint nil)
    "
  ^Navigate^       ^Status^       ^Update^       ^Go To^          ^Dired^
  ^^^^^^^^^^------------------------------------------------------------------------
  _k_: ↑ previous  _t_: todo      _S_: schedule  _g i_: inbox     _g X_: root
  _j_: ↓ next      _→_: right     _D_: deadline  _g w_: work      _g W_: projects
  _c_: archive     _←_: left      _O_: sort      _g p_: personal
  _d_: delete      _,_: priority  _r_: refile    _g j_: journal
  "
    ("<up>" org-previous-visible-heading)
    ("<down>" org-next-visible-heading)
    ("k" org-previous-visible-heading)
    ("j" org-next-visible-heading)
    ("<right>" org-shiftright)
    ("<left>" org-shiftleft)
    ("c" org-archive-subtree)
    ("d" org-cut-subtree)
    ("t" org-todo)
    ("," org-priority)
    ("r" org-refile)
    ("T" org-todo)
    ("S" org-schedule)
    ("D" org-deadline)
    ("O" org-sort)
    ("g i" (find-file org-default-notes-file))
    ("g w" (find-file "~/Dropbox/org/anyone.org"))
    ("g p" (find-file "~/Dropbox/org/personal.org"))
    ("g j" (find-file "~/Dropbox/org/journal.org"))
    ("g X" (dired org-directory))
    ("g W" (dired "~/Dropbox/org/anyone"))
    ("<tab>" (org-cycle))
    ("q" nil "quit"))

(provide 'module-org)
;;; module-org ends here
