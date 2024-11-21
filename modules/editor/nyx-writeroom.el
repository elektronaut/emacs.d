;;; nyx-writeroom.el --- Writeroom -*- lexical-binding: t; -*-

;;; Commentary:
;;;    writeroom-mode is a minor mode for Emacs that implements a
;;;    distraction-free writing mode similar to the famous Writeroom
;;;    editor for OS X.

;;; Code:

;; visual-fill-column-mode is a small Emacs minor mode that mimics the
;; effect of fill-column in visual-line-mode. Instead of wrapping
;; lines at the window edge, which is the standard behaviour of
;; visual-line-mode, it wraps lines at fill-column (or
;; visual-fill-column-width, if set). That is, it turns the view on
;; the left into the view on the right, without changing the contents
;; of the file.
(use-package visual-fill-column
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :after (visual-fill-column))

(provide 'nyx-writeroom)
;;; nyx-writeroom.el ends here
