;; -*- lexical-binding: t; no-byte-compile: t -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(push "lib" straight-default-files-directive)
(straight-use-package 'use-package)

;; replace some package.el functions
(use-package package
  :defer t
  :straight nil
  :config
  (defun package-installed-p (package &optional min-version)
    (when (not (or (stringp package) (symbolp package) (package-desc-p package)))
      (signal 'bad-argument-types `(package ,(type-of package))))
    (let* ((key (cond ((stringp package) package)
                      ((symbolp package) (format "%s" package))
                      ((package-desc-p package) (format "%s" (package-desc-name package)))))
           (plist (gethash key straight--recipe-cache)))
      (plist-get plist :local-repo))))

(require 'bind-key)

(use-package etoile
  :straight (etoile :type git
                    :host github
                    :repo "jsalzbergedu/etoile-emacs"
                    :branch "ubuntu"
                    :no-native-compile t
                    :files ("etoile-emacs/*.el"))
  :demand t)
