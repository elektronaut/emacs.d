;;; core-persp.el --- Perspective
;;; Commentary:
;;; Code:

(require 'dash)
(require 'core-hydra)

;; https://github.com/Bad-ptr/persp-mode.el
(use-package persp-mode
  :ensure t
  :demand t
  :bind (:map persp-key-map
              ("O" . persp-kill-other))
  :init
  (setq-default persp-keymap-prefix (kbd "C-x x")
                persp-add-buffer-on-after-change-major-mode t
                persp-autokill-buffer-on-remove 'kill-weak
                persp-autokill-persp-when-removed-last-buffer 'kill
                persp-kill-foreign-buffer-behaviour-choices 'kill
                persp-remove-buffers-from-nil-persp-behaviour nil)
  :config
  ;;(persp-mode 1)
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))

  ;; Filter out ephemeral buffers
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

  ;; Recent perspectives list
  (setq-default persp-recent-perspectives '())

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

  (add-to-list 'persp-activated-functions
               (lambda (_) (persp-register-recent (persp-name (get-current-persp)))))

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

  (defhydra hydra-persp (:hint nil)
    "
  Perspective: %(safe-persp-name (get-current-persp))

  Navigate^^^^        Manage^^              Buffers
  ^^^^^^^^----------------------------------------------------------
  _←_/_→_: prev/next  _c_: copy             _a_: add
  ^^  _s_: switch     _r_: rename           _k_: remove
  ^^  _h_: hide       _C_: kill             _K_: kill
  ^^  _u_: unhide     _O_: kill all others  _b_: switch
  ^^^^                ^^                    _t_: temporarily display
  "
    ("<left>" persp-prev)
    ("<right>" persp-next)
    ("s" persp-switch)
    ("h" persp-hide)
    ("u" persp-unhide)
    ("c" persp-copy)
    ("r" persp-rename)
    ("C" persp-kill)
    ("O" persp-kill-other)
    ("a" persp-add-buffer)
    ("k" persp-remove-buffer)
    ("K" persp-kill-buffer)
    ("b" persp-switch-to-buffer)
    ("t" persp-temporarily-display-buffer)
    ("q" nil "quit"))

  (define-key persp-mode-map (kbd "C-x X") #'hydra-persp/body))


(provide 'core-persp)
;;; core-persp.el ends here
