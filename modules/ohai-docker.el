;;; -*- lexical-binding: t -*-
;;; ohai-docker.el --- so you can run "not-VMs" inside you VM

(require 'ohai-package)

(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-tramp)

(use-package dockerfile-mode
  :commands dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(provide 'ohai-docker)
