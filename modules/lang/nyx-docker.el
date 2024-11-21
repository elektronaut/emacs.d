;;; nyx-docker.el --- Docker -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t
  :defer 10)

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.yml")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(provide 'nyx-docker)
;;; nyx-docker.el ends here
