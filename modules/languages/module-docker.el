;;; module-docker -- Docker
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package docker
  :ensure t
  :defer 10)

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.yml")

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "Dockerfile\\'")

(provide 'module-docker)
;;; module-docker ends here
