;;; nyx-web.el --- Web mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'"
         "\\.as[cp]x\\'"
         "\\.blade\\.php\\'"
         "\\.erb\\'"
         "\\.es6\\'"
         "\\.html?\\'"
         "\\.jsp\\'"
         "\\.mustache$"
         "\\.phtml\\'"
         "\\.tpl\\'"
         "\\.tpl\\.php\\'")
  :custom ((web-mode-code-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-enable-auto-pairing nil)
           (web-mode-markup-indent-offset 2)
           (web-mode-script-padding 2)
           (web-mode-style-padding 2)))

(provide 'nyx-web)
;;; nyx-web.el ends here
