;;; core-persp.el --- Perspective
;;; Commentary:
;;; Code:

(require 'dash)

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
         (mapc 'persp-kill))))


(provide 'core-persp)
;;; core-persp.el ends here
