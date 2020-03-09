;;; -*- lexical-binding: t -*-
;;; ohai-navigation.el --- Moving around.

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

;; Make PgUp/Dn move the point.
(setq scroll-error-top-bottom t)

;; Avy is a quick way to jump around your buffers.
;; https://github.com/abo-abo/avy
(use-package avy
  :demand t
  :bind (("M-g w" . avy-goto-word-1)
         ("C-:" . avy-goto-char))
  :config
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-;") 'avy-isearch)))

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Consider CamelCase chunks as words when navigating.
(global-subword-mode 1)

;; Enhance C-x o when more than two windows are open.
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; C-c q<left/right> to manage windows history
(winner-mode 1)
;; M-<number> to switch window you want
(use-package winum
  :init
  (progn
    (setq winum-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-`") 'winum-select-window-by-number)
            (define-key map (kbd "C-²") 'winum-select-window-by-number)
            (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
            (define-key map (kbd "M-1") 'winum-select-window-1)
            (define-key map (kbd "M-2") 'winum-select-window-2)
            (define-key map (kbd "M-3") 'winum-select-window-3)
            (define-key map (kbd "M-4") 'winum-select-window-4)
            (define-key map (kbd "M-5") 'winum-select-window-5)
            (define-key map (kbd "M-6") 'winum-select-window-6)
            (define-key map (kbd "M-7") 'winum-select-window-7)
            (define-key map (kbd "M-8") 'winum-select-window-8)
            map))
    (winum-mode)))

;; When using laptop I have fullscreen frame split by 2x2 windows
;; Useing:
;;   C-c w r f - "Window Resize Fullscreen"
;;   C-c w a f - "Window Arrange Four"
(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximized))
(defun my-arrange-windows-4 ()
  (interactive)
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (with-demoted-errors (magit-status-setup-buffer))
  (windmove-right)
  (switch-to-buffer "*Messages*")
  (balance-windows)
  (windmove-up)
  (windmove-left))
(global-set-key (kbd "C-c w r f") 'my-fullscreen)
(global-set-key (kbd "C-c w a f") 'my-arrange-windows-4)

;; Hovever on 25'/2k monitor I prefer to have 3x2 grid
;; Where each window is about 80x35
;; Use:
;;   C-c w r s - "Window Resize Six"
;;   C-c w a s - "Window Arrange Six"
(defun my-resize-6 ()
  (interactive)
  (set-frame-parameter nil 'fullscreen nil)
  (set-frame-size (selected-frame) 300 99))
(defun my-arrange-windows-6 ()
  (interactive)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (windmove-right)
  (with-demoted-errors (magit-status-setup-buffer))
  (windmove-right)
  (switch-to-buffer "*Messages*")
  (balance-windows)
  (windmove-up)
  (windmove-left)
  (windmove-left))
(global-set-key (kbd "C-c w r s") 'my-resize-6)
(global-set-key (kbd "C-c w a s") 'my-arrange-windows-6)

;; Use C-x M-p to kill the buffer in the other window, revealing
;; the next buffer in the stack.
(global-set-key
 (kbd "C-x M-p")
 (lambda () (interactive)
   (save-excursion
     (other-window 1)
     (quit-window))))

;; Display incremental search stats in the modeline.
(use-package anzu
  :demand t
  :config
  (global-anzu-mode 1)
  ;; Anzu provides a version of `query-replace' and friends which give visual
  ;; feedback when composing regexps. Let's replace the regular versions.
  :bind(("C-%" . anzu-query-replace-at-cursor)
        ("M-%" . anzu-query-replace)
        ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode)



(provide 'ohai-navigation)
