;;; module-docker -- Docker
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package docker
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(provide 'module-docker)
;;; module-docker ends here
