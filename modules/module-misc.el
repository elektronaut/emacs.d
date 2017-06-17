;;; module-mic -- Misc modes
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package LaTeX-mode
  :mode "\\.latex\\'"
  :ensure auctex)

(use-package apache-mode
  :mode "\\.htaccess\\'"
  "apache\\.conf\\'"
  "httpd\\.conf\\'"
  "srm\\.conf\\'"
  "access\\.conf\\'"
  "sites-\\(available\\|enabled\\)/")

(use-package cask-mode
  :mode "Cask")

(use-package cmake-mode
  :mode "\\.cmake\\'" "CMakeLists\\.txt\\'"
  :ensure cmake-mode)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package d-mode
  :mode "\\.d\\'")

(use-package dart-mode
  :mode "\\.dart\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(use-package feature-mode
  :mode "\\.feature\\'")

(use-package groovy-mode
  :mode "\\.groovy\\'")

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package kivy-mode
  :mode "\\.kv\\'")

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package nginx-mode)

(use-package php-mode
  :mode "\\.php\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package stylus-mode
  :mode "\\.styl\\'")

(use-package swift-mode
  :mode "\\.swift\\'")

(use-package textile-mode
  :mode "\\.textile\\'")

(use-package thrift-mode
  :mode "\\.thrift\\'"
  :ensure thrift)

(use-package tuareg-mode
  :mode "\\.ml\\'"
  :ensure tuareg)

(provide 'module-misc)
;;; module-misc ends here
