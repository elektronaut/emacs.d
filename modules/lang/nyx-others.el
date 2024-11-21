;;; nyx-others.el --- Other language modes -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Other language modes that do not require configuration.

;;; Code:

(use-package LaTeX-mode
  :ensure auctex
  :mode "\\.latex\\'")

(use-package apache-mode
  :ensure t
  :mode ("\\.htaccess\\'"
         "apache\\.conf\\'"
         "httpd\\.conf\\'"
         "srm\\.conf\\'"
         "access\\.conf\\'"
         "sites-\\(available\\|enabled\\)/"))

(use-package cask-mode
  :ensure t
  :mode "Cask")

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'"
         "CMakeLists\\.txt\\'")
  :ensure cmake-mode)

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package d-mode
  :ensure t
  :mode "\\.d\\'")

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'")

(use-package erlang
  :ensure t
  :mode ("\\.erl\\'" . erlang-mode))

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package kivy-mode
  :ensure t
  :mode "\\.kv\\'")

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package nginx-mode
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package stylus-mode
  :ensure t
  :mode "\\.styl\\'")

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package thrift-mode
  :ensure thrift
  :mode "\\.thrift\\'")

(use-package tuareg-mode
  :ensure tuareg
  :mode "\\.ml\\'")

(use-package vcl-mode
  :ensure t
  :mode "\\.vcl\\'")

(provide 'nyx-others)
;;; nyx-others.el ends here
