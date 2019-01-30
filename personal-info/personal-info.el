;;; personal-info.el --- Personal information. -*- no-byte-compile: t; -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/personal-info
;; Version: 0.1.0
;; Keywords: personal info

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
(defvar personal-info-info nil
  "An alist of personal info.")

(setq personal-info-info
      '((name "[Insert Your Name Here]")
        (email "[Insert Your Email Here]")))

(defun personal-info-get (key)
  "Using KEY, get an item from the personal info list."
  (cadr (assq key personal-info-info)))

(provide 'personal-info)
;;; personal-info.el ends here

;;; personal-info.el ends here
