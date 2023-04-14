;;; core-editing.el --- Editing
;;; Commentary:
;;; Code:

(require 'core-macros)

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


;;-----------------------------------------------------------------------------
;; Packages
;;-----------------------------------------------------------------------------

;; (use-package aggressive-indent
;;   :ensure t
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package anzu
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package darkroom
  :ensure t)

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package move-text
  :ensure t
  :config
  (global-set-key [(control shift up)]  'move-text-up)
  (global-set-key [(control shift down)]  'move-text-down)
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-e"       . mc/edit-lines)
         ("C-S-n"       . mc/mark-next-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" savefile-dir)))

(use-package rainbow-mode
  :ensure t
  :config
  (defun rainbow-turn-off-words ()
    "Turn off word colours in rainbow-mode."
    (interactive)
    (font-lock-remove-keywords
     nil
     `(,@rainbow-x-colors-font-lock-keywords
       ,@rainbow-latex-rgb-colors-font-lock-keywords
       ,@rainbow-r-colors-font-lock-keywords
       ,@rainbow-html-colors-font-lock-keywords
       ,@rainbow-html-rgb-colors-font-lock-keywords))))

(use-package smartrep
  :ensure t)

(use-package operate-on-number
  :ensure t
  :config
  (smartrep-define-key global-map "C-c ."
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("'" . operate-on-number-at-point))))

(use-package string-inflection
  :ensure t)

(use-package tabify
  :ensure nil
  :config
  (with-region-or-buffer untabify))

(use-package viking-mode
  :ensure t
  :config
  (viking-global-mode))

(use-package visual-fill-column
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(provide 'core-editing)
;;; core-editing.el ends here
