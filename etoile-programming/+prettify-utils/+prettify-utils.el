;;; +prettify-utils.el --- Configuration for prettify utils -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/+prettify-utils
;; Version: 0.1.0
;; Keywords: prettify-utils etoile

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
(require 'prettify-utils)

(setq-default prettify-symbols-alist
              (prettify-utils-generate
               ("lambda" "λ")
               ("delta" "∆")
               ("nu" "𝜈")
               ("Reals" "ℝ")
               ("reals" "ℝ")
               ("<="     "≤")
               (">="     "≥")
               ("pi" "𝜋")))


(provide '+prettify-utils)
;;; +prettify-utils.el ends here
