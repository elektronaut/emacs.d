;;; nyx-compilation.el --- Compilation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Built-in package for running compilations and viewing their output
(use-package compile
  :ensure nil
  :config
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error))

(use-package ansi-color
  :ensure nil
  :after (compile)
  :functions (ansi-color-apply-on-region colorize-compilation-buffer)
  :hook ((compilation-filter . colorize-compilation-buffer))
  :config
  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  (defun colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; We don't want to mess with child modes such as grep-mode, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(provide 'nyx-compilation)
;;; nyx-compilation.el ends here
