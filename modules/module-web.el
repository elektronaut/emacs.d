;;; module-web -- Web
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package web-mode
  :mode "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'"
  "\\.as[cp]x\\'"
  "\\.blade\\.php\\'"
  "\\.erb\\'"
  "\\.es6\\'"
  "\\.html?\\'"
  "\\.jsp\\'"
  "\\.jsx\\'"
  "\\.mustache$"
  "\\.phtml\\'"
  "\\.tpl\\'"
  "\\.tpl\\.php\\'"
  :ensure t
  :init
  (setq-default web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-enable-auto-pairing nil
                web-mode-markup-indent-offset 2
                web-mode-script-padding 2
                web-mode-style-padding 2
                web-mode-content-types-alist '(("jsx" . "\\.es6\\'"))))

(provide 'module-web)
;;; module-web ends here
