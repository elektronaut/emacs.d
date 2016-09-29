;;; core-ido -- Ido
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package ido
  :ensure nil
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1)

  (use-package ido-ubiquitous
    :config
    (ido-ubiquitous-mode +1))

  (use-package flx-ido
    :config
    (flx-ido-mode +1)
    (setq ido-use-faces nil))

  (use-package smex
    :init
    (setq smex-save-file (expand-file-name ".smex-items" savefile-dir))
    :config
    (smex-initialize)))

(provide 'core-ido)
;;; core-ido ends here
