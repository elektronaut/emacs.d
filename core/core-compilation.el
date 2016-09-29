;;; core-compilation.el --- Compilation
;;; Commentary:
;;; Code:


(use-package compile
  :ensure nil
  :config
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error)

  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  (defun colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (use-package ansi-color
    :ensure nil
    :config
    (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)))

(provide 'core-compilation)
;;; core-compilation.el ends here
