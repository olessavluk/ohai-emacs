;;; -*- lexical-binding: t -*-
;;; ohai-docker.el --- so you can run "not-VMs" inside you VM

(require 'ohai-package)

(use-package docker-tramp
  :straight (docker-tramp
             :type git
             :host github
             :repo "olessavluk/docker-tramp.el"
             :branch "multi-hops"))

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :commands dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(provide 'ohai-docker)
