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

(defhydra +projectile-hydra (:hint nil :color blue)
    "
^Project Operations^          ^Refactoring^                 ^Navigation^
^------------------^--------+-^----------------^----------+-^----------^---------------------------------
_t_: +projectile-test-hydra | _a_ +projectile-code-action | _g_ counsel-projectile-rg
_c_: +projectile-compile    | ^ ^                         | _p_ counsel-projectile-find-file
^ ^                         | ^ ^                         | _f_ xref-find-definitions
^ ^                         | ^ ^                         | _d_ xref-find-definitions-other-window
^ ^                         | ^ ^                         | _e_ +projectile-flycheck-hydra
"
    ("t" +projectile-test-hydra)
    ("c" +projectile-compile)
    ("g" counsel-projectile-rg)
    ("p" counsel-projectile-find-file)
    ("f" xref-find-definitions)
    ("d" xref-find-definitions-other-window)
    ("e" +projectile-flycheck-hydra/body)
    ("a" +projectile-code-action))


(provide '+projectile)
;;; +projectile.el ends here
