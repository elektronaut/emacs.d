;;; nyx-persp.el --- persp-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Named perspectives(set of buffers/window configs) for Emacs.
;;    https://github.com/Bad-ptr/persp-mode.el

;;; Code:

(require 'dash)
(require 'ibuf-ext)
(require 'nyx-hydra)

(use-package persp-mode
  :ensure t
  :demand t
  :bind (:map persp-key-map
              ("O" . persp-kill-other)
              ("B" . persp-ibuffer))
  :custom ((persp-auto-resume-time 0.0) ;; Do not restore if <= 0
           (persp-keymap-prefix (kbd "C-x x"))
           (persp-add-buffer-on-after-change-major-mode t)
           (persp-autokill-buffer-on-remove 'kill-weak)
           (persp-autokill-persp-when-removed-last-buffer 'kill)
           (persp-kill-foreign-buffer-behaviour 'kill)
           (persp-remove-buffers-from-nil-persp-behaviour nil))
  :config
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))

  ;; Filter out ephemeral buffers
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

  ;; Recent perspectives list
  (defvar persp-recent-perspectives '())

  (defun persp-register-recent (name)
    "Add a perspective to the `persp-register-recent' list."
    (setq persp-recent-perspectives
          (->> (cons name persp-recent-perspectives)
               -distinct
               (-filter (lambda (name) (member name (persp-names)))))))

  (defun persp-names-recent ()
    "Return a list of all perspective names, sorted by recent usage."
    (let* ((all (persp-names))
           (recent (-filter (lambda (name) (member name all)) persp-recent-perspectives)))
      (-distinct (append recent all))))

  (defun persp-register-current-as-recent ()
    "Register the current perspective as recent."
    (let* ((current (get-current-persp))
           (name (if current (persp-name current) "none")))
      (persp-register-recent name)))

  (add-to-list 'persp-activated-functions
               (lambda (_) (persp-register-current-as-recent)))

  (defun persp-kill-empty ()
    "Kill all perspectives without buffers."
    (interactive)
    (->> (persp-names)
         (-map 'persp-get-by-name)
         -flatten
         (-filter (lambda (p) (= 0 (length (persp-buffers p)))))
         (-map 'safe-persp-name)
         (-map 'persp-kill)))

  (defun persp-kill-other ()
    "Kill other perspectives."
    (interactive)
    (->> (persp-names)
         (remove "org")
         (remove "none")
         (remove (safe-persp-name (get-current-persp)))
         (mapc 'persp-kill)))

  ;; ibuffer integration
  (define-ibuffer-filter persp-buffers
      "Show Ibuffer with all buffers in the current perspective."
    (:description "persp-mode"
                  :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
    (with-current-buffer buf
      (find buf (safe-persp-buffers (persp-get-by-name qualifier)))))

  (defun persp-ibuffer-by-persp (persp)
    "Open an IBuffer window showing all buffers in PERSP."
    (ibuffer nil (format "*%s Buffers*" persp)
             (list (cons 'persp-buffers persp))))

  (defun persp-kill-current ()
    "Kill current perspective without prompt."
    (interactive)
    (let ((persp (safe-persp-name (get-current-persp))))
      (persp-prev)
      (persp-kill persp)))

  (defun persp-ibuffer (prompt-for-persp)
    (interactive "P")
    (let ((persp (if prompt-for-persp
                     (completing-read "Select perspective:" (persp-names))
                   (safe-persp-name (get-current-persp)))))
      (persp-ibuffer-by-persp persp))))

(provide 'nyx-persp)
;;; nyx-persp.el ends here
