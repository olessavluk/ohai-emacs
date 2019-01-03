;;; -*- lexical-binding: t -*-
;;; ohai-js-web-mode.el --- Use web-mode to edit JS.

;; Copyright (C) 2015 Bodil Stokke

;; Author: Bodil Stokke <bodil@bodil.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ohai-package)
(require 'ohai-lib)
(require 'ohai-json)

;; If npm is installed, add its local prefix to the executable
;; search path, which helps Emacs find linters etc.
;; This isn't Windows compatible.
;; (-when-let (npm-prefix (ohai/exec-if-exec "npm" "config get prefix"))
;;   (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH"))))

(use-package prettier-js
  :commands prettier-js
  :bind ("C-c C-p" . prettier-js)
  :init (setq-default prettier-js-args '("--single-quote" "true")))

;; Use web-mode for all JS files.
(use-package web-mode
  :mode (("\\.jsx?$" . web-mode)
         ("\\.es6\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx?$")))
  ;; Stop web-mode from using block comments in comment-dwim.
  (setq web-mode-comment-formats
        (-map-when (lambda (i) (equal (car i) "javascript"))
                   (lambda (i) (cons (car i) "//"))
                   web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats `("jsx" . "//"))

  ;; Let Flycheck know that we're using web-mode for JS.
  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  ;; Set up LSP support if the LSP module is loaded.
  (with-eval-after-load "ohai-lsp"
    ;;   (lsp-register-client
    ;;    (make-lsp-client
    ;;     :new-connection (lsp-stdio-connection '("flow-language-server" "--stdio"))
    ;;     :major-modes '(web-mode)
    ;;     :ignore-messages '("readFile .*? requested by Flow but content not available")
    ;;     :server-id 'flow-ls))

    ;; ----------------- result with error
    ;;   error in process filter: Wrong number of arguments: ((t) (_signature-response hover-response) "Handler for `lsp-on-hover-hook'.
    ;; _SIGNATURE-RESPONSE and HOVER-RESPONSE and signature and hover
    ;; data that has been loaded for the current position." (let ((it (let ((result hover-response)) (if result (progn (gethash "contents" result)))))) (if it (lsp-ui-doc--display (thing-at-point (quote symbol) t) (lsp-ui-doc--extract it)) (eldoc-message nil) (lsp-ui-doc--hide-frame)))), 1

    ;; (add-hook 'web-mode-hook 'lsp)
    )
  )


(provide 'ohai-js-web-mode)
