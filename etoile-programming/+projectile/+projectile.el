;;; +projectile.el --- Configuration for projectile -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/+projectile
;; Version: 0.1.0
;; Keywords: projectile configuration

;; This file is not a part of GNU Emacs

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

;;; Commentary:

;;; Code:
(require 'projectile)
(require 'counsel-projectile)
(require 'subr-x)
(require 'hydra)
(require 'flycheck)

(defvar +projectile-local-commands '()
  "An alist of plists mapping project types to commands used on a project.")

(defun +projectile-local-commands-add-type (type)
  "Add a project TYPE to +projectile-local-commands."
  (push (list type) +projectile-local-commands))

(defun +projectile-local-commands-add-command (type command-type command)
  (setcdr (assoc type +projectile-local-commands)
          `(,command-type ,command ,@(alist-get type +projectile-local-commands))))

(defun +projectile-get-local-command (command)
  "Using a project of type TYPE, get the COMMAND."
  (plist-get (alist-get (projectile-project-type) +projectile-local-commands) command))

(defun +projectile-test-hydra ()
  "Open the test hydra for the mode"
  (interactive)
  (funcall (+projectile-get-local-command :test-hydra)))

(defun +projectile-compile ()
  "Compile the project"
  (interactive)
  (funcall (+projectile-get-local-command :compile)))

(defun +projectile-code-action ()
  "Compile the project"
  (interactive)
  (funcall (+projectile-get-local-command :code-action)))

(defhydra +projectile-flycheck-hydra (:hint nil :color blue)
    "
_n_: flycheck-next-error
_N_: flycheck-previous-error
_e_: flycheck-list-errors
"
    ("n" flycheck-next-error :color pink)
    ("N" flycheck-previous-error :color pink)
    ("e" flycheck-list-errors :color blue))

(defun +projectile-google-scholar (string)
  (interactive "Msearch string: ")
  (let ((string (url-encode-url string)))
    (w3m (format "https://scholar.google.com/scholar?q=%s" string))))

(defun copy-citation-title ()
    (interactive)
    (ivy-read "Copy citation: " (create-citation-option-list (get-buffer-citations))
              :initial-input nil
              :dynamic-collection nil
              :action (lambda (pair) (kill-new (cdr pair)))))

(defun get-buffer-citations ()
    (json-read-from-string (shell-command-to-string (concat "anystyle.sh " (shell-quote-argument buffer-file-name)))))

(defun sort-citation-results (citations)
  (sort citations (lambda (x y) (let* ((num-x (cdr-safe (car (-filter (lambda (x) (eq (car x) 'citation-number)) x))))
                                  (num-y (cdr-safe (car (-filter (lambda (y) (eq (car y) 'citation-number)) y))))
                                  (num-x (and (arrayp num-x) (aref num-x 0)))
                                  (num-y (and (arrayp num-y) (aref num-y 0)))
                                  (num-x (or (and (stringp num-x) (string-to-number num-x)) 1.0e+INF))
                                  (num-y (or (and (stringp num-y) (string-to-number num-y)) 1.0e+INF))
                                  (num-x (or (and (numberp num-x) (not (= num-x 0)) num-x) 1.0e+INF))
                                  (num-y (or (and (numberp num-y) (not (= num-y 0)) num-y) 1.0e+INF)))
                             (< num-x num-y)))))

(defun create-citation-option-list (cl)
  (cl-loop for citation across cl
           collect `(,(format "%s: %s by %s"
                              (car (-map 'cdr-safe (-filter (lambda (x) (eq (car x) 'citation-number)) citation)))
                              (car (-map (lambda (x) (or (and x (aref x 0)) "No Title")) (-map 'cdr-safe (-filter (lambda (x) (eq (car x) 'title)) citation))))
                              (car (-map (lambda (x) (seq-reduce (lambda (a z) (format "%s %s;%s" (alist-get 'family z) (alist-get 'given z) a)) x "")) (-map 'cdr-safe (-filter (lambda (x) (eq (car x) 'author)) citation)))))
                     .
                     ,(format "%s" (aref (or (car (-map 'cdr-safe (-filter (lambda (x) (eq (car x) 'title)) citation))) ["No Title"]) 0)))))


(defhydra +projectile-hydra (:hint nil :color blue)
    "
^Project Operations^          ^Refactoring^                 ^Navigation^
^------------------^--------+-^----------------^----------+-^----------^---------------------------------
_t_: +projectile-test-hydra | _a_ +projectile-code-action | _g_ counsel-projectile-rg
_c_: +projectile-compile    | ^ ^                         | _p_ counsel-projectile-find-file
_n_: org-capture            | ^ ^                         | _f_ xref-find-definitions
_s_: google scholar         | ^ ^                         | _d_ xref-find-definitions-other-window
_i_: copy citation          | ^ ^                         | _e_ +projectile-flycheck-hydra
"
    ("t" +projectile-test-hydra)
    ("c" +projectile-compile)
    ("g" counsel-projectile-rg)
    ("p" counsel-projectile-find-file)
    ("f" xref-find-definitions)
    ("d" xref-find-definitions-other-window)
    ("e" +projectile-flycheck-hydra/body)
    ("a" +projectile-code-action)
    ("n" org-capture)
    ("s" +projectile-google-scholar)
    ("i" copy-citation-title))


(provide '+projectile)
;;; +projectile.el ends here
