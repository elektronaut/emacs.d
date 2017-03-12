;;; core-term.el --- Terminal
;;; Commentary:
;;; Code:

(defface term-color-black
  '((t (:foreground "#181e26" :background "#21242b")))
  "Black terminal color.")
(defface term-color-red
  '((t (:foreground "#ff6c6b" :background "#21242b")))
  "Red terminal color.")
(defface term-color-green
  '((t (:foreground "#98be65" :background "#21242b")))
  "Green terminal color.")
(defface term-color-yellow
  '((t (:foreground "#ECBE7B" :background "#21242b")))
  "Yellow terminal color.")
(defface term-color-blue
  '((t (:foreground "#51afef" :background "#21242b")))
  "Blue terminal color.")
(defface term-color-magenta
  '((t (:foreground "#c678dd" :background "#21242b")))
  "Magenta terminal color.")
(defface term-color-cyan
  '((t (:foreground "#46D9FF" :background "#21242b")))
  "Cyan terminal color.")
(defface term-color-white
  '((t (:foreground "#DFDFDF" :background "#21242b")))
  "White terminal color.")

'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

(setq ansi-term-color-vector
      [term term-color-black term-color-red term-color-green term-color-yellow
            term-color-blue term-color-magenta term-color-cyan term-color-white])

(provide 'core-term)
;;; core-term.el ends here
