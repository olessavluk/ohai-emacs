;;; -*- lexical-binding: t -*-
;;; ohai-typescript.el --- TypeScript language support

;;; Code:

;; (use-package typescript-mode
;;   :mode ("\\.tsx?$" . typescript-mode))


(use-package tide
  :after (web-mode ;; typescript-mode
          company flycheck)
  :hook ((web-mode . tide-setup)
         (web-mode . tide-hl-identifier-mode)
         ;; (typescript-mode . tide-setup)
         ;; (typescript-mode . tide-hl-identifier-mode)
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
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  ;; (add-hook 'typescript-mode-hook #'setup-tide-mode))
  )



(provide 'ohai-typescript)
