;;; +emacs.el --- Configuration for emacs variables -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/+emacs
;; Version: 0.1.0
;; Keywords: configuration etoile

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
;; Configuration for emacs variables

;;; Code:
(use-package f
  :straight t
  :demand t)

(defun +emacs/search (string)
  (interactive "sSearch: ")
  (start-process "Search the internet"
                 nil
                 "firefox"
                 "--search"
                 string))


(defun write-temp-and-kill ()
  "Write the region to a temp file, then add the temp file the kill ring."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
        (file (make-temp-file "write-temp-and-kill"))
        (coding 'utf-8))
    (f-write text 'utf-8 file)
    (kill-new file)))

;; "Opt out" of custom-set-variables.
;; These variables will still be viewable after they are set,
;; but they will dissapear upon the cleaning of /tmp/.
(setq custom-file "/tmp/emacs-custom.el")

;; Enable some disabled methods
(put 'upcase-region 'disabled nil)

;; Increase recentf size
(setq recentf-max-saved-items 100)

(put 'narrow-to-region 'disabled nil)

(provide '+emacs)
;;; +emacs.el ends here
