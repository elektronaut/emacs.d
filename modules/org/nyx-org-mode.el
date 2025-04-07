;;; nyx-org-mode.el --- org-mode config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-mobile)
(require 'org-refile)
(require 'ob-ruby)
(require 'nyx-consult)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org-archive\\'" . org-mode))

(keymap-global-set "C-c l" 'org-store-link)

(setopt org-directory "~/Library/CloudStorage/Dropbox/org"
        org-archive-location "%s-archive::datetree/"

        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file

        org-cycle-separator-lines 2
        org-cycle-open-archived-trees t
        org-default-notes-file (concat org-directory "/inbox.org")
        org-enforce-todo-dependencies nil
        org-ellipsis nil
        org-extend-today-until 3
        org-global-properties (quote (("Effort_ALL" .
                                       "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))
        org-hide-leading-stars nil
        org-log-done 'time

        org-outline-path-complete-in-steps nil
        org-tags-exclude-from-inheritance '("project")
        org-replace-disputed-keys t
        org-reverse-note-order t
        org-src-fontify-natively t
        org-edit-src-content-indentation 0
        org-startup-folded 'show2levels
        org-startup-indented t
        org-id-link-to-org-use-id t
        org-return-follows-link t)

;; Open file links in same window
(setopt org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame)))

(setopt org-mobile-directory "~/Library/CloudStorage/Dropbox/Apps/MobileOrg"
        org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))

(setopt org-startup-truncated nil
        visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'org-mode-hook #'visual-line-mode)

(setopt org-stuck-projects
        '("+project/-DONE-MAYBE-DELEGATED-CANCELLED" ("NEXT" "WAITING") nil ""))

(setopt org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAITING" "MAYBE" "|"
                    "DONE(d)" "DELEGATED" "CANCELLED")))

(setopt org-tag-alist (quote ((:startgroup)
                              ("@errand"  . ?e)
                              ("@office"  . ?o)
                              ("@home"    . ?h)
                              (:endgroup)
                              ("project"  . ?p)
                              ("meeting"  . ?m)
                              ("maybe"    . ?M)
                              ("note"     . ?n)
                              ("review"   . ?r)
                              ("tasks"    . ?t)
                              ("flagged"  . ??))))

(with-eval-after-load 'org
  ;; Prevent Org from overriding the bindings for windmove.
  (keymap-unset org-mode-map "S-<left>")
  (keymap-unset org-mode-map "S-<right>")
  (keymap-unset org-mode-map "S-<up>")
  (keymap-unset org-mode-map "S-<down>")
  (keymap-unset org-agenda-mode-map "S-<up>")
  (keymap-unset org-agenda-mode-map "S-<down>")
  (keymap-unset org-agenda-mode-map "S-<left>")
  (keymap-unset org-agenda-mode-map "S-<right>")
  (keymap-set org-mode-map "C-c :" 'org-time-stamp-inactive)

  ;; Calendar navigation
  (keymap-set org-read-date-minibuffer-local-map "M-<left>"
              (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (keymap-set org-read-date-minibuffer-local-map "M-<right>"
              (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (keymap-set org-read-date-minibuffer-local-map "M-<up>"
              (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (keymap-set org-read-date-minibuffer-local-map "M-<down>"
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
  (keymap-set org-mode-map "C-<left>" 'org-shiftleft)
  (keymap-set org-mode-map "C-<right>" 'org-shiftright)
  (keymap-set org-agenda-mode-map "C-<left>" 'org-agenda-do-date-earlier)
  (keymap-set org-agenda-mode-map "C-<right>" 'org-agenda-do-date-later))

(defun nyx-external-link-opener (protocol)
  "Return a function that will open PROTOCOL URLs."
  `(lambda (path)
     (start-process-shell-command
      "open-org-process" nil
      (concat "/usr/bin/open " (shell-quote-argument (concat ,protocol ":" path))))))

(dolist (protocol '("zpl" "readdle-spark"))
  (org-link-set-parameters protocol :follow (nyx-external-link-opener protocol)))

(provide 'nyx-org-mode)
;;; nyx-org-mode.el ends here
