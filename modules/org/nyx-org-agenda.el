;;; nyx-org-agenda-el --- org-mode agenda -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'nyx-org-mode)

(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-block-separator 8212
      org-agenda-compact-blocks nil
      org-agenda-files (cl-remove-if-not
                        #'file-directory-p
                        (directory-files-recursively
                         org-directory ".*" t
                         (lambda (dir) (not (string-match-p "/data$" dir)))))
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

;; From https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
(defun nyx-org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
A block is identified as empty if there are fewer than 2 non-empty lines
in the block (excluding the line with `org-agenda-block-separator'
characters)."
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

(add-hook 'org-agenda-finalize-hook #'nyx-org-agenda-delete-empty-blocks)

(provide 'nyx-org-agenda)
;;; nyx-org-agenda.el ends here
