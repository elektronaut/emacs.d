;;; nyx-css.el --- CSS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar-local css-base-rem-size 16
  "Base font size in pixels for px to rem conversion.")

(defun css-toggle-px-rem ()
  "Toggle between px and rem for number at point or in region.
Uses `css-base-rem-size' (defaults to 16) as the base for conversion."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (number-str (when bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (base (buffer-local-value 'css-base-rem-size (current-buffer))))
    (when (and bounds number-str)
      (let* ((is-px (string-match-p "px$" number-str))
             (is-rem (string-match-p "rem$" number-str))
             (num (string-to-number number-str))
             (new-str (cond
                       (is-px (format "%grem" (/ num (float base))))
                       (is-rem (format "%dpx" (round (* num base))))
                       (t number-str))))
        (save-excursion
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert new-str))))))

(use-package css-mode
  :ensure nil
  :bind (("C-c C-t" . css-toggle-px-rem))
  :mode (("\\.css\\'"     . scss-mode)
         ("\\.postcss\\'" . scss-mode)
         ("\\.pcss\\'"    . scss-mode)
         ("\\.scss\\'"    . scss-mode))
  :custom ((css-indent-offset 2)
           (css-fontify-colors nil))
  :hook ((css-mode  . rainbow-mode)
         (scss-mode . rainbow-mode)))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(provide 'nyx-css)
;;; nyx-css.el ends here
