;;; nyx-hippie-expand.el --- hippie-expand -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package hippie-expand
  :ensure nil
  :bind (("s-/" . hippie-expand)
         ("M-/" . hippie-expand))
  :custom ((hippie-expand-try-functions-list '(try-expand-dabbrev
                                               try-expand-dabbrev-all-buffers
                                               try-expand-dabbrev-from-kill
                                               try-complete-file-name-partially
                                               try-complete-file-name
                                               try-expand-all-abbrevs
                                               try-expand-list
                                               try-expand-line
                                               try-complete-lisp-symbol-partially
                                               try-complete-lisp-symbol))))

(provide 'nyx-hippie-expand)
;;; nyx-hippie-expand.el ends here
