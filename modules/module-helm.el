;;; module-helm -- Helm
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package helm
  :bind (("C-c h" . helm-command-prefix)
         ("M-i"   . helm-swoop)
         ("M-I"   . helm-swoop-back-to-last-point)
         ("C-x w" . helm-spaces)
         :map helm-command-map
         ("o"     . helm-occur)
         ("g"     . helm-do-grep)
         ("C-c w" . helm-wikipedia-suggest)
         ("SPC"   . helm-all-mark-rings))
  :init
  :config
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t)
  (use-package helm-projectile)
  (use-package helm-spaces)
  (use-package helm-swoop))

(provide 'module-helm)
;;; module-helm ends here
