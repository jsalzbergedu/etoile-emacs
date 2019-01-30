;;; passwords.el --- Passwords for this config -*- no-byte-compile: t; -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/passwords
;; Version: 0.1.0
;; Keywords: etoile passwords

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
;; Passwords for etoile.

;;; Code:

(defvar passwords-passwords '()
  "An alist of passwords.")

(setq passwords-passwords
      '((gitter . "[Insert your gitter password here]")
        (bintray-user . "[Insert your bintray username here]")
        (bintray-key . "[Insert your bintray key here]")
        (irc . "[Insert your irc password here]")))

(defun passwords-get (key)
  "Return the password as a string at KEY."
  (cdr (assoc key passwords-passwords)))

(provide 'passwords)
;;; passwords.el ends here
