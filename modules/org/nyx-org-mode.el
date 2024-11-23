;;; nyx-org-mode.el --- org-mode config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-mobile)
(require 'org-refile)
(require 'ob-ruby)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org-archive\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)

(setq org-directory "~/Library/CloudStorage/Dropbox/org"
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
      org-log-done t

      org-outline-path-complete-in-steps nil
      org-tags-exclude-from-inheritance '("project")
      org-replace-disputed-keys t
      org-reverse-note-order t
      org-src-fontify-natively t
      org-startup-folded "overview"
      org-startup-indented t
      org-id-link-to-org-use-id t
      org-return-follows-link t)

(setq org-mobile-directory "~/Library/CloudStorage/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))

(setq org-startup-truncated nil
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'org-mode-hook #'visual-line-mode)

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

(setq org-tag-alist (quote ((:startgroup)
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
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<up>") nil)
  (define-key org-agenda-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<left>") nil)
  (define-key org-agenda-mode-map (kbd "S-<right>") nil)
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
