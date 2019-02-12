;;; etoile-apps.el --- Configuration of applications for etoile

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/etoile-apps
;; Version: 0.1.0
;; Keywords: etoile apps

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
;; Configuration of applications for etoile

;;; Code:
(use-package passwords
  :straight (passwords :type git
                       :host github
                       :repo "jsalzbergedu/etoile-emacs"
                       :files ("passwords/*"))
  :demand t)

(use-package dired-subtree
  :straight (dired-hacks :type git
                         :host github
                         :repo "Fuco1/dired-hacks")
  :demand t
  :general
  (:keymaps '(dired-mode-map)
            "i" 'dired-subtree-insert))

;; Org
;; TODO split into + packages
(use-package git
  :straight t
  :defer t)

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

(use-package org
  :defer t
  :straight nil
  :init
  (straight-use-package 'org-plus-contrib)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :config
  (setq org-indent-indentation-per-level 1
	org-ellipsis ":"
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-heading-line t
	org-startup-indented t
        org-src-fontify-natively t)
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                           (emacs-lisp . t)
                                                           (scheme . t)
                                                           (scala . t)
                                                           (coq . t)
                                                           (haskell . t)))
  :commands org-mode)

(use-package ob-plantuml
  :init
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :straight nil
  :demand t
  :after org)

(use-package org-evil
  :straight (org-evil :type git
                      :host github
                      :repo "GuiltyDolphin/org-evil")
  :demand t
  :after org)

(use-package org-evil-motion
  :demand t
  :after org-evil
  :straight nil)

(use-package tramp
  :straight nil
  :defer t
  :config
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
  (add-to-list 'backup-directory-alist
	       (cons tramp-file-name-regexp nil)))

;; Silver Searcher
(use-package ag
  :straight (ag :type git
                :host github
                :repo "Wilfred/ag.el")
  :defer t)

;; ERC irc client
;; TODO add + package, filter out personal info
(use-package erc
  :straight t
  :init
  ;; (setq erc-autojoin-channels-alist (list (cons "freenode.net" (list "#stratis-storage" "#scheme")) (cons "mozilla.org" (list "#rust" "#rust-beginners" "#servo"))))
  ;; The above line got annoying
  (setq erc-prompt (concat "<jcob>:"))
  (defun my-erc-connect ()
    "Connect to the IRC servers I usually connect to"
    (interactive)
    ;; (erc :server "irc.freenode.net"
    ;;      :port 6667
    ;;      :nick "jcob"
    ;;      :password (passwords-get 'irc)
    ;;      :full-name "Jacob Salzberg")
    ;; (erc :server "irc.mozilla.org" :port 6667 :nick "jcob" :full-name "Jacob Salzberg"))
    (erc :server "localhost" :port 5008
         :nick "jcob"
         ;; :password (passwords-get 'irc)
         :full-name "Jacob Salzberg"))
  (setq erc-autojoin-mode t
        erc-button-mode t
        erc-fill-mode t
        erc-irccontrols-mode t
        erc-list-mode t
        erc-match-mode t
        erc-menu-mode t
        erc-move-to-prompt-mode t
        erc-netsplit-mode t
        erc-networks-mode t
        erc-noncommands-mode t
        erc-pcomplete-mode t
        erc-readonly-mode t
        erc-ring-mode t
        erc-stamp-mode t
        erc-track-minor-mode t))

(use-package ansi-term
  :straight nil
  :defer t
  :general
  (:keymaps '(ansi-term) :states '(normal motion)
            "p" 'term-paste)
  :config (add-hook 'term-mode-hook (lambda ()
				      (evil-local-set-key 'normal (kbd "p") 'term-paste))))

;; Gitter
(use-package gitter
  :defer t
  :straight (gitter :type git
                    :host github
                    :repo "xuchunyang/gitter.el")
  :init (setq gitter-token (passwords-get 'gitter)))

;; PDF tools
(use-package pdf-tools
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  :general
  (:keymaps '(pdf-view-mode-map)
            "J" 'pdf-view-shrink
            "K" 'pdf-view-enlarge)
  :commands pdf-view-mode)

;; eww
(use-package eww
  :straight nil
  :defer t
  :config
  (add-hook 'eww-mode-hook (lambda () (setq word-wrap t))))

;; Get back to exwm later

(use-package exwm
  :straight t
  :defer t)

(use-package exwm-config
  :demand t
  :after (exwm)
  :config
  (advice-add 'exwm-config-ido :override (lambda () t))
  (push (cons (kbd "<escape>") #'evil-normal-state) exwm-input-global-keys)
  :commands exwm-config-default)

(provide 'etoile-apps)
;;; etoile-apps.el ends here
