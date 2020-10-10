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
                persp-autokill-buffer-on-remove 'kill-weak
                persp-autokill-persp-when-removed-last-buffer 'kill)
  :config
  ;;(persp-mode 1)
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))

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



;; ;; https://github.com/nex3/perspective-el
;; (use-package perspective
;;   :ensure t
;;   :bind (:map persp-mode-map
;;               ("C-x x C" . persp-kill-other))
;;   :config
;;   (persp-mode)
;;   (defun persp-kill-other ()
;;     "Kill other perspectives."
;;     (interactive)
;;     (mapc 'persp-kill (remove (persp-name (persp-curr))
;;                               (remove "org" (persp-names))))))
;;
;; (use-package persp-projectile
;;   :ensure t
;;   :after (perspective))

(provide 'core-persp)
;;; core-persp.el ends here
