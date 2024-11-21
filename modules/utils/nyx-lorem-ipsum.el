;;; nyx-lorem-ipsum.el --- Dummy text -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Generate dummy text
(use-package lorem-ipsum
  :ensure t
  :custom (lorem-ipsum-sentence-separator " "))

(provide 'nyx-lorem-ipsum)
;;; nyx-lorem-ipsum.el ends here
