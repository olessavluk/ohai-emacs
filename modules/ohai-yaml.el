;;; -*- lexical-binding: t -*-
;;; ohai-yaml.el --- YAML Ain't Markup Language

(require 'ohai-package)


(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode)))


(provide 'ohai-yaml)
