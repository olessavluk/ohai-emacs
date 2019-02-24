;;; -*- lexical-binding: t -*-
;;; ohai-typescript.el --- TypeScript language support

;;; Code:

(use-package typescript-mode
  :mode ("\\.tsx?$" . typescript-mode))

(use-package tide
  :after (
          ;; not sure why it is hanging with web-mode?
          ;; web-mode
          typescript-mode
          company
          flycheck)
  :hook ((web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode))))
         (typescript-mode . setup-tide-mode)
         ;; (before-save . tide-format-before-save)
         )
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))



(provide 'ohai-typescript)
