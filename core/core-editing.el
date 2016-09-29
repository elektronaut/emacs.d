;;; core-editing.el --- Editing
;;; Commentary:
;;; Code:

;; Autosave
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Whitespace
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq tab-always-indent 'complete)

;; Default mode
(custom-set-variables '(default-major-mode 'text-mode))

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)

(set-default 'imenu-auto-rescan t)

(use-package tabify
  :ensure nil
  :config
  (with-region-or-buffer untabify))

(with-region-or-buffer indent-region)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
    (mapcar (lambda (fn)
              (let ((name (car fn)))
                (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                    (cons
                     (match-string 1 name)
                     (cons (string-to-number (match-string 2 name))
                           (string-to-number (or (match-string 3 name) ""))))
                  fn))) files)))


(provide 'core-editing)
;;; core-editing.el ends here
