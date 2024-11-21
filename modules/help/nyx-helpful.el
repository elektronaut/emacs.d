;;; nyx-helpful.el --- Helpful -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h h" . helpful-at-point)))

(provide 'nyx-helpful)
;;; nyx-helpful.el ends here
