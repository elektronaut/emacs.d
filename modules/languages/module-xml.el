;;; module-xml -- XML
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-auto-insert-xml-declaration-flag nil
      nxml-bind-meta-tab-to-complete-flag t
      nxml-slash-auto-complete-flag t)

(provide 'module-xml)
;;; module-xml ends here
