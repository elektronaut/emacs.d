;;; nyx-xml.el --- XML -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setopt nxml-child-indent 4
        nxml-attribute-indent 4
        nxml-auto-insert-xml-declaration-flag nil
        nxml-bind-meta-tab-to-complete-flag t
        nxml-slash-auto-complete-flag t)

(provide 'nyx-xml)
;;; nyx-xml.el ends here
