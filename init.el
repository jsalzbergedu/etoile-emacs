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

(use-package etoile
  :straight (etoile :type git
                    :host github
                    :repo "jsalzbergedu/etoile-emacs"
                    :files ("etoile-emacs/*.el"))
  :demand t)
