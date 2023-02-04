;;; module-org -- Org mode
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'core-hydra)
(require 'core-projectile)
(require 'core-secrets)
(require 'org)
(require 'org-agenda)
(require 'ob-ruby)

(defun skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
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
(global-set-key "\C-cc" 'my-org-capture)

(setq org-directory "~/Library/CloudStorage/Dropbox/org"
      org-archive-location "%s-archive::datetree/"
      org-cycle-separator-lines 2
      org-cycle-open-archived-trees t
      org-default-notes-file "~/Library/CloudStorage/Dropbox/org/inbox.org"
      org-enforce-todo-dependencies nil
      org-ellipsis nil
      org-global-properties (quote (("Effort_ALL" .
                                     "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit")))
      org-hide-leading-stars nil
      org-log-done t
      org-mobile-directory "~/Library/CloudStorage/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")
      org-outline-path-complete-in-steps nil
      org-tags-exclude-from-inheritance '("project")
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-replace-disputed-keys t
      org-reverse-note-order t
      org-src-fontify-natively t
      org-startup-folded "overview"
      org-startup-indented t
      org-id-link-to-org-use-id t
      org-return-follows-link t)

(setq org-agenda-block-separator 8212
      org-agenda-compact-blocks nil
      org-agenda-files (directory-files-recursively org-directory "\\.org$")
      org-agenda-persistent-filter t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-span 14)

(setq org-agenda-custom-commands
      '(("n" "Next actions"
         ((agenda "" ((org-agenda-span 7)))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High priority")))
          (tags-todo "+inbox"  ((org-agenda-overriding-header "Inbox")))
          (todo "NEXT"  ((org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'scheduled 'deadline
                                                     'regexp "\\=.*\\[#A\\]"))
                         (org-agenda-overriding-header "Next actions")))
          (stuck ""  ((org-agenda-overriding-header "Stuck projects")))))
        ("p" "Projects"
         ((stuck ""  ((org-agenda-overriding-header "Stuck projects")))
          (tags "project/-DONE-MAYBE-DELEGATED-CANCELLED-WAITING"
                ((org-agenda-overriding-header "Active projects")))
          (tags "project/+WAITING"
                ((org-agenda-overriding-header "Upcoming projects")))
          (tags "project/+MAYBE"
                ((org-agenda-overriding-header "Potential projects")))))
        ("w" "Waiting"
         ((todo "WAITING" ((org-agenda-overriding-header "Waiting")))
          (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))
          (todo "MAYBE" ((org-agenda-overriding-header "Someday/maybe")))))
        ("e" "Effort"
         ((todo "NEXT|TODO"
                ((org-agenda-overriding-header "Tasks")
                 (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
                 (org-agenda-prefix-format "  %5e %-10c ")))))
        ("o" "Overview"
         ((agenda "" ((org-agenda-span 14)))
          (stuck "" ((org-agenda-overriding-header "Stuck projects")))
          (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
          (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))
          (todo "TODO" ((org-agenda-overriding-header "Actionable tasks")))
          (todo "MAYBE" ((org-agenda-overriding-header "Someday/maybe")))))))

(setq org-stuck-projects
      '("+project/-DONE-MAYBE-DELEGATED-CANCELLED" ("NEXT" "WAITING") nil ""))

(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING" "MAYBE" "|"
                  "DONE(d)" "DELEGATED" "CANCELLED")))

(let ((waiting (face-attribute 'font-lock-constant-face :foreground))
      (maybe (face-attribute 'font-lock-variable-name-face :foreground)))
  (setq org-todo-keyword-faces
        `(("WAITING" :foreground ,waiting :weight bold)
          ("MAYBE"   :foreground ,maybe :weight bold))))

(setq my-org-capture-default-target
      "~/Library/CloudStorage/Dropbox/org/inbox.org")

(defvar my-org-capture-inbox-target
  my-org-capture-default-target
  "Filename for `org-capture'.")

(defvar my-org-capture-email-target
  my-org-capture-default-target
  "Filename for `org-capture'.")

(setq org-capture-templates
      '(("t" "Task" entry (file+olp my-org-capture-inbox-target "Tasks")
	 "* TODO %?\n%U\n%i" :prepend t)
        ("T" "Task (with link)" entry (file+olp my-org-capture-inbox-target "Tasks")
	 "* TODO %?\n%U\n%a\n%i" :prepend t)
        ("n" "Note" entry (file+olp my-org-capture-inbox-target "Notes")
	 "* %? %U\n%i" :prepend t)
        ("s" "Source note" entry (file+olp my-org-capture-inbox-target "Notes")
	 "* %? %U\n#+BEGIN_SRC\n%i\n#+END_SRC\nFrom: %a" :prepend t)
        ("j" "Journal entry" entry (file+datetree "~/Library/CloudStorage/Dropbox/org/journal.org")
	 "* %?\n%i")
        ("m" "Email workflow")
        ("mf" "Follow up" entry (file+olp my-org-capture-email-target "Email")
         "* NEXT Follow up with %:fromname on %a\n\n%i"
         :prepend t :immediate-finish t)
        ("mr" "Read later" entry (file+olp my-org-capture-email-target "Email")
         "* NEXT Read %a\n\n%i" :prepend t :immediate-finish t)))

(defun my-org-capture ()
  "Set `my-org-capture-inbox-target' and call `org-capture'."
  (interactive)
  (setq my-org-capture-inbox-target
        (if (and (projectile-project-p)
                 (file-exists-p (projectile-org-file)))
            (projectile-org-file)
          my-org-capture-default-target))
  (call-interactively #'org-capture))

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

  ;; Calendar navigation
  (define-key org-read-date-minibuffer-local-map (kbd "M-<left>")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<right>")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<up>")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<down>")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))

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

(use-package org-mime
  :ensure t
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

;; (use-package org-journal
;;   :init
;;   (setq org-journal-dir "~/Library/CloudStorage/Dropbox/org/journal"))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-dir (expand-file-name "org-gcal/" savefile-dir)
;;         org-gcal-client-id (config-secret 'gcal-client-id)
;; 	org-gcal-client-secret (config-secret 'gcal-client-secret)
;; 	org-gcal-file-alist '(("inge@anyone.no" .  "~/Library/CloudStorage/Dropbox/org/gcal-anyone.org")))
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

(defun external-link-opener (protocol)
  "Return a function that will open PROTOCOL URLs."
  `(lambda (path)
     (start-process-shell-command
      "open-org-process" nil
      (concat "/usr/bin/open " (shell-quote-argument (concat ,protocol ":" path))))))

(dolist (protocol '("zpl" "readdle-spark"))
  (org-link-set-parameters protocol :follow (external-link-opener protocol)))

(defhydra hydra-org (:hint nil)
    "
  Navigate^^^^                 Status^^^^        Update^^              Insert^^^^                    Go To
  ^^^^^^^^^^^^-------------------------------------------------------------------------------------------------------------
      _↑_/_↓_: heading         ^^_n_: NEXT       _a_: toggle archive   _i t_/_i T_: time (inactive)  _g i_: inbox
    M-_↑_/_↓_: move            ^^_t_: TODO       _A_: move to archive  _i d_/_i D_: date (inactive)  _g a_: anyone
    M-_←_/_→_: shift           ^^_d_: DONE       _r_: refile           ^^^^                          _g p_: personal
  S-M-_←_/_→_: shift subtree   ^^_T_: todo       _S_: schedule         ^^^^                          _g j_: journal
  ^^^^                       _←_/_→_: prev/next  _D_: deadline         ^^^^                          _g A_: areas
  ^^^^                         ^^_,_: priority   _O_: sort             ^^^^                          _g C_: clients

 "
    ;; Navigate
    ("<up>" org-previous-visible-heading)
    ("<down>" org-next-visible-heading)
    ("M-<down>" org-move-subtree-down)
    ("M-<up>" org-move-subtree-up)
    ("M-<left>" org-metaleft)
    ("M-<right>" org-metaright)
    ("S-M-<left>" org-shiftmetaleft)
    ("S-M-<right>" org-shiftmetaright)
    ;; Status
    ("<right>" org-shiftright)
    ("<left>" org-shiftleft)
    ("n" (org-todo "NEXT"))
    ("t" (org-todo "TODO"))
    ("d" (org-todo "DONE"))
    ("T" org-todo)
    ("," org-priority)
    ;; Update
    ("a" org-toggle-archive-tag)
    ("A" org-archive-to-archive-sibling)
    ("r" org-refile)
    ("S" org-schedule)
    ("D" org-deadline)
    ("O" org-sort)
    ;; Insert
    ("i d" org-time-stamp)
    ("i D" org-time-stamp-inactive)
    ("i t" (org-time-stamp t))
    ("i T" (org-time-stamp-inactive t))
    ;; Go to
    ("g i" (find-file org-default-notes-file))
    ("g a" (find-file "~/Library/CloudStorage/Dropbox/org/areas/anyone.org"))
    ("g p" (find-file "~/Library/CloudStorage/Dropbox/org/personal.org"))
    ("g j" (find-file "~/Library/CloudStorage/Dropbox/org/journal.org"))
    ("g O" (dired org-directory))
    ("g A" (dired "~/Library/CloudStorage/Dropbox/org/areas"))
    ("g C" (dired "~/Library/CloudStorage/Dropbox/org/clients"))
    ;; Misc
    ("k" org-cut-subtree "delete")
    ("<tab>" (org-cycle))
    ("q" nil "quit"))

(define-key org-mode-map (kbd "C-c O") #'hydra-org/body)
(define-key org-mode-map (kbd "C-c o") nil)

(provide 'module-org)
;;; module-org.el ends here
