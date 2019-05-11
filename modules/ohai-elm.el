;;; -*- lexical-binding: t -*-
;;; ohai-elm.el --- FIRE ALL MONAD TRANSFORMERS

;;; Code:

(use-package elm-mode
  :commands elm-mode
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)))

(use-package flycheck-elm
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)))


(provide 'ohai-elm)
