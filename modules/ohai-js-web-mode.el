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
  :bind ("C-c C-p" . ohai/prettier-js)
  ;; :init (setq-default prettier-js-args '("--single-quote" "true"))
  :config
  (defun ohai/prettier-js ()
    "Same as default, but using node_modules executable first"
    (interactive)
    (setq-local prettier-js-command (ohai/resolve-node-exec "prettier"))
    (prettier-js)))

;; Use web-mode for all JS files.
(use-package web-mode
  :mode (("\\.tsx?$" . web-mode)
         ("\\.jsx?$" . web-mode)
         ("\\.es6\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :bind ("C-c r" . ohai/web-mode-reload)
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

  (defun ohai/web-mode-reload ()
    (interactive)
    (web-mode-reload)
    (flycheck-mode +1)
    (company-mode +1))
  )

(use-package groovy-mode)

;; If the LSP module is enabled
;; (with-eval-after-load "ohai-lsp"
;;   (add-hook 'web-mode-hook #'lsp))

(provide 'ohai-js-web-mode)
