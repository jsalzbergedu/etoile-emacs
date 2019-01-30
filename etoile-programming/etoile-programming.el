;;; etoile-programming.el --- Programming configuration for etoile -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/etoile-programming
;; Version: 0.1.0
;; Keywords: etoile programming

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
;; Programming configuration for etoile

;;; Code:

;; Remove tabs
(setq-default indent-tabs-mode nil)

;; Relevant to all programming before language packages are setup:
(defvar prog-minor-modes-common (list)
  "A common hook for programming minor modes")
(defun prog-minor-modes-common ()
  "A common hook for programming minor modes"
  (interactive)
  (mapc 'funcall prog-minor-modes-common))
(defun add-prog-minor-modes-common (&rest mode-hooks)
  "Add prog-minor-modes-common to MODE-HOOKS"
  (mapc (lambda (a) (add-hook a 'prog-minor-modes-common)) mode-hooks))

;; Whitespace detection
(defun show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(push 'show-trailing-whitespace prog-minor-modes-common)

;; Ansi coloring
(use-package ansi-color
  :demand t
  :straight nil)

;; Cookiecutter to quickly start projects
(use-package cookiecutter
  :straight (cookiecutter :type git
                          :host github
                          :repo "jsalzbergedu/cookiecutter-emacs-ui")
  :defer t)

;; Compile for... compilation
(use-package compile
  :after ansi-color
  :defer t
  :straight nil
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  :bind (:map compilation-mode-map
	      ("SPC" . nil)))

;; Prettify symbols
(use-package prettify-utils
  :demand t
  :straight (prettify-utils :type git
                            :host github
                            :repo "Ilazki/prettify-utils.el"
                            :fork (:host github
                                   :repo "jsalzbergedu/prettify-utils.el")))

(use-package ansi-color
  :demand t
  :straight nil)

;; Cookiecutter to quickly start projects
(use-package cookiecutter
  :straight (cookiecutter :type git
                          :host github
                          :repo "jsalzbergedu/cookiecutter-emacs-ui")
  :defer t)

;; Compile for... compilation
(use-package compile
  :after ansi-color
  :defer t
  :straight nil
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  :bind (:map compilation-mode-map
	      ("SPC" . nil)))
(provide 'etoile-programming)
;;; etoile-programming.el ends here
