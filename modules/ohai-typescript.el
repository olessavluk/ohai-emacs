;;; -*- lexical-binding: t -*-
;;; ohai-typescript.el --- TypeScript language support

;;; Code:

(use-package typescript-mode
  :mode ("\\.ts$" . typescript-mode))

(use-package tide
  ;; :after (web-mode)
  ;; :after (
  ;;         ;; not sure why it is hanging with web-mode?
  ;;         ;; web-mode
          ;; typescript-mode
          ;; company
  ;;         flycheck)
  :hook (
         ;; (web-mode . (lambda ()
         ;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
         ;;                 (setup-tide-mode))))
         (web-mode . setup-tide-mode)
         (typescript-mode . setup-tide-mode)
         ;; (before-save . tide-format-before-save)
         )
  :bind ("C-c w t" . setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (flycheck-select-checker 'javascript-eslint)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'javascript-eslint 'tsx-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'typescript-tide 'append))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

;; If the LSP module is enabled
;; (with-eval-after-load "ohai-lsp"
;;   (add-hook 'typescript-mode-hook #'lsp))

(provide 'ohai-typescript)
