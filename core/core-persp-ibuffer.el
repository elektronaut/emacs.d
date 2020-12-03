;;; core-persp-ibuffer.el --- persp-mode ibuffer integration
;;; Commentary:
;;; Code:

(require 'core-persp)

(with-eval-after-load "ibuffer"

  (require 'ibuf-ext)

  (define-ibuffer-filter persp
      "Toggle current view to buffers of current perspective."
    (:description "persp-mode"
                  :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
    (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

  (defun persp-add-ibuffer-group ()
    (let ((perspslist (list
                       (list (safe-persp-name (get-frame-persp))
                             (cons 'persp (safe-persp-name (get-frame-persp)))))))
      (setq ibuffer-saved-filter-groups
            (delete* "persp-mode" ibuffer-saved-filter-groups
                     :test 'string= :key 'car))
      (push
       (cons "persp-mode" perspslist)
       ibuffer-saved-filter-groups)))

  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (persp-add-ibuffer-group)
                (ibuffer-switch-to-saved-filter-groups "persp-mode"))))

(provide 'core-persp-ibuffer)
;;; core-persp-ibuffer.el ends here
