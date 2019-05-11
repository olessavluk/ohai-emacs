;;; -*- lexical-binding: t -*-
;;; ohai-orgmode.el --- Your personal everything manager.

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

;; straight.el issue when using org
;; see https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(straight-use-package 'org-plus-contrib)

;; Fancy bullet rendering.
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Insert links from clipboard.
(use-package org-cliplink
  :config
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink)))

;; Always use visual-line-mode in org-mode, and wrap it at column 80.
;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;    (visual-line-mode 1)
;;    (set-visual-wrap-column 80)))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-directory "~/org")
(setq org-index-file "~/org/index.org")
(setq org-archive-location
      (concat "~/org/archive.org" "::* From %s"))

;; add CLOSE: <time> when completing todo
(setq org-log-done 'time)

(setq org-agenda-files (list org-directory))
(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file "~/org/blog-ideas.org")
         "* %?\n")

        ("e" "Email" entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n\n%a\n\n")

        ("f" "Finished book"
         table-line (file "~/org/books-read.org")
         "| %^{Title} | %^{Author} | %u |")

        ("r" "Reading"
         checkitem
         (file "~/org/to-read.org"))

        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")))

(use-package ox-jira)

(provide 'ohai-orgmode)
