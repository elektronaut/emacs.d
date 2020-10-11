;;; nyx-font-size.el --- Font size
;;; Commentary:
;;; Code:

;; Big font mode

(defvar nyx-big-font-adjust 40
  "Adjusted font size for big-font-mode.")

(define-minor-mode big-font-mode
  "Enable big fonts."
  :global t
  :lighter " big"
  :after-hook
  (nyx-set-font-size))

;; Monitor based font size

(defvar nyx-frame-monitor-name nil
  "Name of current monitor.")

(defvar nyx-frame-monitor-hook nil
  "Hook called after frame changes monitor.")

(defvar nyx-monitor-font-sizes
  '(("Color LCD" . 130) ; Macbook Pro
    (default . 120))
  "Font size mapping for monitors.")

(defun nyx-detect-frame-monitor-change ()
  "Detects when frame change monitor."
  (let ((monitor-name  (cdr (assq 'name (frame-monitor-attributes)))))
    (unless (string-equal nyx-frame-monitor-name monitor-name)
      (progn
        (setq nyx-frame-monitor-name monitor-name)
        (run-hooks 'nyx-frame-monitor-hook)))))

(defvar nyx-frame-monitor-timer
  (run-with-timer 1 2 'nyx-detect-frame-monitor-change))

;; Typography
(defun nyx-set-font-size ()
  "Configures default font size based on current display."
  (interactive)
  (let* ((monitor-name  (cdr (assq 'name (frame-monitor-attributes))))
         (base-font-size (cdr (or (assoc monitor-name nyx-monitor-font-sizes)
                                  (assoc 'default nyx-monitor-font-sizes))))
         (font-size (if big-font-mode (+ base-font-size nyx-big-font-adjust)
                      base-font-size)))
    (dolist (face '(default variable-pitch))
      (set-face-attribute face nil
                          :family "JetBrains Mono"
                          :height font-size)))
  ;(setq-default line-spacing 4)
  (setq-default line-spacing 3))

(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)

(nyx-set-font-size)
(add-hook 'window-configuration-change-hook 'nyx-set-font-size)
(add-hook 'nyx-frame-monitor-hook 'nyx-set-font-size)

;; Line spacing for the minibuffer
(defun nyx-minibuffer-line-spacing ()
  (setq-local line-spacing 4))
(add-hook 'minibuffer-setup-hook #'nyx-minibuffer-line-spacing)




(provide 'nyx-font-size)
;;; nyx-font-size.el ends here
