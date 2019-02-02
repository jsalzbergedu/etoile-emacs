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
;;;###autoload
(defvar prog-minor-modes-common (list)
  "A common hook for programming minor modes")
;;;###autoload
(defun prog-minor-modes-common ()
  "A common hook for programming minor modes"
  (interactive)
  (mapc 'funcall prog-minor-modes-common))
;;;###autoload
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
  :general
  (:keymaps '(compilation-mode-map)
            "SPC" nil))

;; Prettify symbols
(use-package prettify-utils
  :demand t
  :straight (prettify-utils :type git
                            :host github
                            :repo "Ilazki/prettify-utils.el"
                            :fork (:host github
                                   :repo "jsalzbergedu/prettify-utils.el")))

(use-package +prettify-utils
  :demand t
  :after (prettify-utils)
  :straight (+prettify-utils :type git
                             :host github
                             :repo "jsalzbergedu/etoile-emacs"
                             :files ("etoile-programming/+prettify-utils/*.el"))
  :config
  (add-hook 'prog-minor-modes-common 'prettify-symbols-mode))

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


;; Display side indicators in the margins
(use-package fringe-helper
  :demand t
  :straight t)

;; Smartparens, for () {} '' "" []
(use-package smartparens
  :straight (smartparens :type git
                         :host github
                         :repo "Fuco1/smartparens")
  :defer t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-minor-modes-common 'show-paren-mode)
  :commands (smartparens-mode sp-forward-slurp-sexp)
  :init
  (add-hook 'prog-minor-modes-common 'smartparens-mode)
  (add-hook 'prog-minor-modes-common 'show-smartparens-mode)
  :general
  (:keymaps '(normal motion)
            "SPC s" 'sp-forward-slurp-sexp)
  :bind (:map evil-normal-state-map
	      ("SPC s" . sp-forward-slurp-sexp)
	      :map evil-motion-state-map
	      ("SPC s" . sp-forward-slurp-sexp)))

(use-package smartparens-config
  :straight nil
  :after smartparens
  :demand t)

;; Rainbow delimiters, a visual hint of nest depth
(use-package rainbow-delimiters
  :defer t
  :straight (rainbow-delimeters :type git
                                :host github
                                :repo "Fanael/rainbow-delimiters")
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-minor-modes-common 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :straight t
  :init (add-hook 'prog-minor-modes-common 'rainbow-mode)
  :config
  ;; Remove the #define being colored
  (setq rainbow-hexadecimal-colors-font-lock-keywords
        '(("[^&]\\(#(?!define)\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
           (1 (rainbow-colorize-itself 1)))
          ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)(?!define)+\\{1,4\\}\\)"
           (0 (rainbow-colorize-itself)))
          ("[Rr][Gg][Bb]:[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}"
           (0 (rainbow-colorize-itself)))
          ("[Rr][Gg][Bb][Ii]:[0-9.]+/[0-9.]+/[0-9.]+"
           (0 (rainbow-colorize-itself)))
          ("\\(?:[Cc][Ii][Ee]\\(?:[Xx][Yy][Zz]\\|[Uu][Vv][Yy]\\|[Xx][Yy][Yy]\\|[Ll][Aa][Bb]\\|[Ll][Uu][Vv]\\)\\|[Tt][Ee][Kk][Hh][Vv][Cc]\\):[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?"
           (0 (rainbow-colorize-itself))))))

;; Yasnippet, a snippet manager
(use-package yasnippet
  :defer t
  :straight (yasnippet :type git
                       :host github
                       :repo "joaotavora/yasnippet")
  :commands yas-insert-snippet)

(use-package yasnippet-snippets
  :after yasnippet
  :demand t
  :straight (yasnippet-snippets :type git
                                :host github
                                :repo "AndreaCrotti/yasnippet-snippets"
                                :files ("yasnippet-snippets.el" "snippets")))

(add-hook 'prog-minor-modes-common 'yas-minor-mode)
(add-hook 'prog-minor-modes-common 'evil-normalize-keymaps)

;; Company mode for autocompletion

(use-package company
  :demand t
  :straight (company :type git
                     :host github
                     :repo "company-mode/company-mode")
  :init (global-company-mode 1)
  :config
  (defhydra company-hydra (:hint nil :color blue)
    "
Navigation
^^^^^^^^^^^--------------
_k_: company-select-previous-or-abort
_j_: company-select-next-or-abort
"
    ("k" company-select-previous-or-abort :color 'pink)
    ("j" company-select-next-or-abort :color 'pink))
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "C-o") 'company-hydra/body)
  :commands global-company-mode)

;; LSP mode, a common interface to LSP providers
;; The lsp modes don't play nice with lazy loading
(use-package lsp-mode
  :demand t
  :straight (lsp-mode :type git
                      :host github
                      :repo "emacs-lsp/lsp-mode")
  :init
  (setq lsp-prefer-flymake nil)
  :config
  (setq lsp-inhibit-message t
        lsp-print-io nil))

;; LSP's completion package
(use-package company-lsp
  :demand t
  :straight (company-lsp :type git
                         :host github
                         :repo "tigersoldier/company-lsp")
  :config (add-to-list 'company-backend 'company-lsp))

;; Required by dap-mode
(use-package bui
  :straight (bui :type git
                 :host github
                 :repo "alezost/bui.el")
  :demand t)

;; Required by dap-mode
(use-package tree-mode
  :straight t
  :demand t)

;; Like LSP, but for debuggers
(use-package dap-mode
  :demand t
  :straight (dap-mode :type git
                      :host github
                      :repo "yyoncho/dap-mode"
                      :files ("*.el" "icons/eclipse")))

(use-package dap-ui
  :after dap-mode
  :straight nil)

(use-package dap-ui-repl
  :after dap-mode
  :straight nil)

(use-package dap-hydra
  :after dap-mode
  :straight nil)

(use-package personal-info
  :demand t
  :straight (personal-info :type git
                           :host github
                           :repo "jsalzbergedu/etoile-emacs"
                           :files ("personal-info/*.el")))

(use-package project-init
  :straight (project-init :type git
                          :host github
                          :repo "jsalzbergedu/project-init")
  :init (progn
            (setq project-init-author-email (personal-info-get 'email)
                  project-init-author-name (personal-info-get 'name))))

;; Ripgrep for fast grepping through projects
(use-package ripgrep
  :defer t
  :straight t)

(use-package projectile
  :demand t
  :straight t
  :init
  (put 'projectile-project-root 'safe-local-variable #'stringp)
  :config
  (projectile-global-mode 1))

(use-package counsel-projectile
  :straight t
  :demand t)

;; Project hyrdra for generating hydras for projects
(use-package project-hydra
  :demand t
  :straight (project-hydra :type git
                           :host github
                           :repo "jsalzbergedu/project-hydra")
  :init
  (eval-and-compile
    (require 'project-hydra)))

;; Magit, the excellent interface to git through emacs
(use-package magit
  :defer t
  :straight t
  :general
  (:keymaps '(magit-mode-map magit-diff-mode-map)
            "SPC" nil))

;; Evil-magit, the only way I can use magit
(use-package evil-magit
  :straight (evil-magit :type git
                        :host github
                        :repo "emacs-evil/evil-magit")
  :demand t
  :after magit)

(use-package keychain-environment
  :straight t
  :demand t
  :config
  (keychain-refresh-environment))

(use-package flycheck
  :straight t
  :defer t
  :config
  (setq flycheck-idle-change-delay 2)
  (defhydra hydra-flycheck-error (:hint nil :color blue)
    "
_n_: flycheck-next-error
_N_: flycheck-previous-error
_e_: flycheck-list-errors
"
    ("n" flycheck-next-error :color pink)
    ("N" flycheck-previous-error :color pink)
    ("e" flycheck-list-errors :color blue)))

;; LSP integration with flycheck
(use-package lsp-ui
  :demand t
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (set-face-attribute 'lsp-ui-sideline-code-action nil
                      :foreground "#2ecc71")

  (set-face-attribute 'lsp-ui-sideline-current-symbol nil
                      :foreground "white"
                      :box '(:line-width -1 :color "white")
                      :weight 'ultra-bold
                      :height 0.95)

  (set-face-attribute 'lsp-ui-sideline-symbol nil
                      :foreground "#7f8c8d"
                      :box '(:line-width -1 :color "#7f8c8d")
                      :height 0.9)
  :commands lsp-ui-mode)

(use-package lsp-ui-flycheck
  :demand t
  :straight nil
  :config (add-hook 'lsp-after-open-hook
                    (lambda () (lsp-ui-flycheck-enable 1))))


;; Nlinum to display the line
(use-package nlinum
  :straight t
  :init (add-hook 'prog-minor-modes-common 'nlinum-mode)
  :config (progn (setq nlinum-format "%4d │ "))
  :commands nlinum-mode)

(use-package nlinum-hl
  :straight (emacs-nlinum-hl :type git
                             :host github
                             :repo "hlissner/emacs-nlinum-hl")
  :after nlinum
  :demand t)

;; All c-likes
(use-package google-c-style
  :defer t
  :straight (google-c-style :type git
                            :host github
                            :repo "google/styleguide"
                            :branch "gh-pages"
                            :files ("*.el")))

;; Python:
(use-package python
  :defer t
  :straight nil
  :init
  (add-hook 'python-mode-hook 'prog-minor-modes-common)
  (add-hook 'python-mode-hook 'lsp))

;; Java
;; TODO clean these up into + packages
(use-package output-buffer
  :straight (output-buffer :type git
                           :host github
                           :repo "jsalzbergedu/output-buffer")
  :defer t)

(use-package elisp-checkstyle
  :straight (elisp-checkstyle :type git
                              :host github
                              :repo "jsalzbergedu/elisp-checkstyle")
  :defer t
  :config
  (setq checkstyle-executable "~/cs-checkstyle/checkstyle")
  :commands (checkstyle-curr-p checkstyle-output-curr))

(defun checkstyle ()
  (interactive)
  (checkstyle-output-curr))

(use-package gradle-mode
  :defer t
  :straight (emacs-gradle-mode :type git
                               :host github
                               :repo "jacobono/emacs-gradle-mode")
  :after elisp-checkstyle)

;; dap-jave sneakily uses cl by using first, most probably
(use-package cl
  :straight nil
  :demand t
  :commands first)

(use-package dap-java
  :after (dap-mode lsp-java cl)
  :init
  (defhydra dap-java-testrun-hydra (:hint nil :color blue)
    "
^Debug^                         ^Run Test^                          ^Other^
-----------------------------------------------------------------------------------
_d_: dap-debug-java             _r c_: dap-java-run-test-class     _h_ dap-hydra
_c_: dap-java-run-test-class    _r m_: dap-java-run-test-method
_m_: dap-java-run-test-method"
    ("d" dap-debug-java)
    ("c" dap-java-debug-test-class)
    ("m" dap-java-debug-test-method)
    ("r c" dap-java-run-test-class)
    ("r m" dap-java-run-test-method)
    ("h" dap-hydra))
  :config
  (evil-define-key 'normal dap-ui-repl-mode-map (kbd "<return>")
    #'comint-send-input)
  (evil-define-key 'insert dap-ui-repl-mode-map (kbd "<return>")
    (lambda ()
      (interactive)
      (insert "\n")))
  (add-hook 'dap-ui-repl-mode-hook 'evil-normalize-keymaps))

(defvar material-design-icons-repo-location "~/sources/material-design-icons/")
(defvar treemacs-material-design-icons-alist '())
(setq treemacs-material-design-icons-alist
      (-map (lambda (x) (cons (car x) (expand-file-name (cdr x) material-design-icons-repo-location)))
            '((dir-open . "file/svg/production/ic_folder_open_24px.svg")
              (dir-closed . "file/svg/production/ic_folder_24px.svg")
              (node-closed . "navigation/svg/production/ic_expand_less_18px.svg")
              (node-open . "navigation/svg/production/ic_chevron_right_18px.svg"))))


(defvar material-design-icons-community-repo-location "~/sources/MaterialDesign/")

(defvar material-design-icons-community-repo-location "~/sources/MaterialDesign/")
(defvar treemacs-material-design-icons-community-alist '())
(setq treemacs-material-design-icons-community-alist
      (-map (lambda (x) (cons (car x) (expand-file-name (cdr x) material-design-icons-community-repo-location)))
            '((package . "icons/svg/package.svg")
              (leaf . "icons/svg/circle-small.svg")
              (java . "~/sources/MaterialDesign/icons/svg/language-java.svg"))))

(defun cdr-assoc (&rest args)
  "Get the CDR of the ASSOC'd list"
  (cdr (apply #'assoc args)))

;; (defmacro setup-icons (&rest icon-list)
;;   "Set up all the icons, turning a into the icon repo location
;; and b into the community repo location.
;; Set up in triples of variables: (vairable-to-set 'some-key <a or b>)"
;;   (-map (lambda (x)
;;           `(treemacs--setup-icon ,(car x) (cdr-assoc ,(cadr x) ,(if (equalp (caddr x) 'a)
;;                                                                     material-design-icons-community-repo-location
;;                                                                   material-design-icons-repo-location)))))


(use-package treemacs
  :straight t
  :config
  (treemacs--setup-icon treemacs-icon-tag-node-closed-png (cdr-assoc 'node-closed treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-tag-node-open-png (cdr-assoc 'node-open treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-open-png (cdr-assoc 'dir-open treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-closed-png (cdr-assoc 'dir-closed treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-tag-leaf-png (cdr-assoc 'leaf treemacs-material-design-icons-community-alist))
  (treemacs--setup-icon treemacs-icon-root-png (cdr-assoc 'package treemacs-material-design-icons-community-alist))
  (treemacs--setup-icon treemacs-icon-java (cdr-assoc 'java
                                                      treemacs-material-design-icons-community-alist) "java")
  (setq treemacs-git-mode 'simple)
  (add-hook 'treemacs-mode-hook (lambda () (setq-local truncate-lines t)))
  :commands treemacs--setup-icon)


(define-minor-mode java-project-mode
  "A mode which is activated whenever the java-project variable is t."
  :lighter ""
  :keymap (make-sparse-keymap))

(add-hook 'java-project-mode-hook 'evil-normalize-keymaps)

(project-hydra hydra-java
  :test dap-java-testrun-hydra/body
  :compile lsp-java-build-project
  :stylecheck checkstyle
  :search counsel-projectile-rg
  :git magit-status
  :run dap-java-debug
  :and ("p" counsel-projectile-find-file)
  :and ("e" hydra-flycheck-error/body)
  :and ("a" lsp-ui-sideline-apply-code-actions)
  :and ("d" xref-find-definitions-other-window)
  :and ("f" xref-find-definitions)
  :and ("o" toggle-dap))

;; (evil-define-key 'normal java-project-mode-map (kbd "SPC p") 'hydra-java/body)

(defvar-local is-java-project nil "A local variable that, when set to t,
allows java-project-mode-global to be activated.")

(put 'is-java-project 'safe-local-variable #'booleanp)

(define-globalized-minor-mode java-project-mode-global java-project-mode
  (lambda ()
    (when (or (locate-dominating-file default-directory "pom.xml")
              (locate-dominating-file default-directory ".classpath")
              is-java-project)
        (java-project-mode 1))))


(use-package lsp-java
  :demand t
  :straight (lsp-java :type git
                      :host github
                      :repo "emacs-lsp/lsp-java"
                      :files ("*.el" "icons" "install"))
  :config
  (java-project-mode-global 1)
  (progn (add-hook 'java-mode-hook 'prog-minor-modes-common)
	 (add-hook 'java-mode-hook 'lsp)
	 (add-hook 'java-mode-hook (lambda ()
				                 (flycheck-mode 1)
				                 (google-set-c-style)
				                 (google-make-newline-indent)
				                 (setq indent-tabs-mode nil
					               tab-width 4
                                                       c-basic-offset 4))))

  (evil-define-key 'normal java-mode-map (kbd "SPC p") 'hydra-java/body))

(use-package lsp-java-treemacs
  :demand t
  :straight nil)

;; Groovy
(use-package groovy-mode
  :straight (groovy-mode :type git
                         :host github
                         :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :config (progn (add-hook 'groovy-mode-hook 'prog-minor-modes-common)
		 (add-to-list 'auto-mode-alist '("\\build.gradle\\'" . groovy-mode)))
  :defer t)

;; Scala
(use-package sbt-mode
  :straight (ensime-sbt-mode
             :type git
             :host github
             :repo "ensime/emacs-sbt-mode")
  :defer t)


(use-package scala-mode
  :defer t
  :straight (scala-mode :type git
                        :host github
                        :repo "ensime/emacs-scala-mode")
  :config (progn (add-hook 'scala-mode-hook 'prog-minor-modes-common)
		 (setq scala-indent:add-space-for-scaladoc-asterisk nil)))

(use-package ensime
  :straight (ensime :type git
                    :host github
                    :repo "ensime/ensime-emacs"
                    :branch "2.0")
  :defer t
  :commands ensime
  :config (setq ensime-startup-notification nil))

;; Elisp:
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
                                  :host github
                                  :repo "purcell/exec-path-from-shell")
  :demand t
  :config (exec-path-from-shell-initialize))

(add-hook 'emacs-lisp-mode-hook 'prog-minor-modes-common)

(defun eval-region-advice (eval-region-orig start end &optional printflag read-function)
  (funcall eval-region-orig start end t read-function))
(advice-add 'eval-region :around 'eval-region-advice)

(defun mark-region-with-face (face)
  (interactive "SFace: ")
  (put-text-property (region-beginning) (region-end) 'face face))
;; Common Lisp:
(use-package slime-autoloads
  :demand t
  :straight (slime-autoloads :type git
                             :host github
                             :repo "slime/slime"
                             :files ("slime-autoloads.el")))

(use-package slime
  :defer t
  :straight (slime :type git
                   :host github
                   :repo "slime/slime"
                   :files ("*"))
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (if (file-exists-p (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
      (load (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
    (when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (evil-define-key 'visual slime-mode-map "SPC e" 'slime-eval-region)
  (add-prog-minor-modes-common 'lisp-mode-hook 'slime-repl-mode-hook)
  :init
  (add-hook 'lisp-mode-hook 'prog-minor-modes-common))

;; Scheme
(use-package scheme-complete
  :straight t
  :defer t)

(use-package geiser
  :defer t
  :straight t
  :init (add-hook 'scheme-mode-hook (lambda ()
				      (geiser-mode)
				      (prog-minor-modes-common)))
  :config (progn (setq geiser-active-implementations '(chicken))
		 (add-prog-minor-modes-common 'scheme-mode-hook 'geiser-repl-mode-hook)
                 (add-to-list 'auto-mode-alist '("\\.setup\\'" . scheme-mode))
                 (add-to-list 'auto-mode-alist '("\\.meta\\'" . scheme-mode)))
  :commands geiser-mode)

;; Rust:
(define-minor-mode rust-project-mode
  "A mode which is activated whenever the is-rust-project variable is t."
  :lighter ""
  :keymap (make-sparse-keymap))

(add-hook 'rust-project-mode-hook 'evil-normalize-keymaps)

(evil-define-key 'normal rust-project-mode-map (kbd "SPC p") 'hydra-rust/body)

(defvar-local is-rust-project nil "A local variable that, when set to t,
allows rust-project-mode-global to be activated.")

(put 'is-rust-project 'safe-local-variable #'booleanp)

(define-globalized-minor-mode rust-project-mode-global rust-project-mode
  (lambda ()
    (when (or (locate-dominating-file default-directory "Cargo.toml")
              is-rust-project)
        (rust-project-mode 1))))

(rust-project-mode-global 1)

(use-package rust-mode
  :defer t
  :straight (rust-mode :type git
                       :host github
                       :repo "rust-lang/rust-mode"
                       :files ("rust-mode.el"))
  :config
  (add-hook 'rust-mode-hook 'prog-minor-modes-common)
  (add-hook 'rust-mode-hook (lambda ()
                              (add-to-list 'flycheck-checkers 'lsp-ui)))
  (add-hook 'rust-mode-hook 'lsp)

  (project-hydra hydra-rust
    :test cargo-process-test
    :compile cargo-process-build
    :stylecheck nil
    :search counsel-projectile-rg
    :git hydra-magit/body
    :run cargo-process-run
    :and ("p" counsel-projectile-find-file)
    :and ("e" hydra-flycheck-error/body)
    :and ("a" lsp-ui-sideline-apply-code-actions)
    :and ("d" xref-find-definitions-other-window)
    :and ("f" xref-find-definitions)))

(use-package flycheck-rust
  :defer t
  :straight (flycheck-rust :type git
                           :host github
                           :repo "flycheck/flycheck-rust"))

(use-package cargo
  :defer t
  :straight (cargo :type git
                   :host github
                   :repo "kwrooijen/cargo.el"))

;; Redox:
;; I'll get back to rdxmk later

;; Toml:
(use-package toml-mode
  :defer t
  :straight (toml-mode :type git
                       :host github
                       :repo "dryman/toml-mode.el")
  :config (progn (add-hook 'toml-mode-hook 'prog-minor-modes-common)))

;; TeX:

;; HTML:
(use-package sgml-mode
  :defer t
  :straight nil
  :config (progn (add-hook 'sgml-mode-hook 'prog-minor-modes-common)))

;; Markdown
(use-package markdown-mode
  :defer t
  :straight t
  :config (progn (setq markdown-command "/usr/bin/pandoc")
		 (add-hook 'markdown-mode-hook 'prog-minor-modes-common)))

;; Shell
(use-package sh-script
  :defer t
  :straight nil
  :config (progn (add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))
		 (add-hook 'sh-mode-hook 'prog-minor-modes-common))
  :commands sh-mode)

;; Debugging
(use-package realgud
  :straight t
  :defer t
  :commands realgud:gdb
  :config
  ;; I would really like to be able to set this to nil,
  ;; but doing so crashes emacs
  (setq realgud-bp-use-fringe t))

(use-package cmake-mode
  :defer t
  :straight t
  :config
  (add-hook 'cmake-mode-hook 'prog-minor-modes-common))

;; TODO: Move all the c++ stuff into + packages

(use-package flycheck-vera
  :demand t
  :after flycheck
  :straight (flycheck-vera :type git
                           :host github
                           :repo "jsalzbergedu/flycheck-vera")
  :config
  (flycheck-add-next-checker 'lsp-ui 'c/c++-vera++)
  ;; List of rules can be found on
  ;; https://bitbucket.org/verateam/vera/wiki/Rules
  ;; No trailing whitespace
  (push "L001" flycheck-vera-rules)
  ;; No tab characters
  (push "L002" flycheck-vera-rules)
  ;; No leading and no trailing empty lines
  (push "L003" flycheck-vera-rules)
  ;; Line cannot be too long
  (push "L004" flycheck-vera-rules)
  ;; There should not be too many consecutive empty lines
  (push "L005" flycheck-vera-rules)
  ;; Source file should not be too long
  (push "L006" flycheck-vera-rules)
  ;; One line comments should not have forced continuation
  (push "T001" flycheck-vera-rules)
  ;; Reserved names should not be used for preprocessor macros
  (push "T002" flycheck-vera-rules)
  ;; Some keywords should be followed by a single space
  (push "T003" flycheck-vera-rules)
  ;; Some keywords should be immediatley followed by a colon
  (push "T004" flycheck-vera-rules)
  ;; Keywords break and continue should immediatley be followed by a semicolon
  (push "T005" flycheck-vera-rules)
  ;; Keywords return and throw should be immediatley followed by a semicolon
  ;; or a single space
  (push "T006" flycheck-vera-rules)
  ;; Semicolons should not be isolated by spaces or comments from the
  ;; rest of the code
  (push "T007" flycheck-vera-rules)
  ;; Keywords catch, for, if, and switch should be followed by a single space
  (push "T008" flycheck-vera-rules)
  ;; Commma should not be preceded by whitespace, but should be followed by one
  (push "T009" flycheck-vera-rules)
  ;; Identifiers should not be composed of 'l' and 'O' characters only
  (push "T010" flycheck-vera-rules)
  ;; Curly brakets from the same pair should be either in the same line or in
  ;; the same column
  ;; (push "T011" flycheck-vera-rules)
  ;; Negation operator should not be used in its short form
  ;; (push "T012" flycheck-vera-rules)
  ;; Source files should contain the copyright notice
  ;; (push "T013" flycheck-vera-rules)
  ;; Source files should refer the Boost Software License
  ;; (push "T014" flycheck-vera-rules)
  ;; HTML links in commments and string literals should be correct
  (push "T015" flycheck-vera-rules)
  ;; Calls to min/max should be protected against accidental macro substitution
  ;; (push "T016" flycheck-vera-rules)
  ;; Unnamed namespaces are not allowed in header files
  (push "T017" flycheck-vera-rules)
  ;; "using namespace" is not allowed in header files
  (push "T018" flycheck-vera-rules)
  ;; Control structures must have complete curly-braced block of code
  (push "T019" flycheck-vera-rules)
  ;; My own rule:
  ;; Brackets must be in kernel c style
  (push "LINUXKERNELBRACKETS.py" flycheck-vera-rules))

(use-package ccls
  :defer t
  :after flycheck
  :init
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-sem-highlight-method 'overlay)
  :straight t
  :config
  ;; (setq ccls-sem-function-colors
  ;;       '("#34495e" "#34495e" "#34495e" "#34495e" "#34495e"
  ;;         "#34495e" "#34495e" "#34495e" "#34495e" "#34495e"))
  (setq ccls-sem-highlight-method 'overlay)
  (ccls-use-default-rainbow-sem-highlight)
  (defun ccls--is-ccls-buffer-advice (f &rest args)
    "Fix ccls--is-ccls-buffer."
    (apply f args)
    (locate-dominating-file default-directory ".ccls-root"))

  (set-face-attribute 'ccls-code-lens-face nil
                      :inherit 'shadow
                      :height 0.8)
  (advice-add 'ccls--is-ccls-buffer :around 'ccls--is-ccls-buffer-advice)
  :commands (lsp-ccls-enable ccls-code-lens-mode))
;;   :commands lsp-cquery-enable)

(defun +ccls/enable ()
  (interactive)
  (require 'ccls)
  (lsp)
  (flycheck-mode))

(defun +ccls/enable-with-lens ()
  (+ccls/enable)
  (ccls-code-lens-mode))

;; TODO: Figure out how to have both cpplint and vera
;; (use-package flycheck-google-cpplint
;;   :demand t
;;   :after (flycheck lsp-ui)
;;   :straight (flycheck-google-cpplint :type git
;;                                      :host github
;;                                      :repo "flycheck/flycheck-google-cpplint"
;;                                      :fork (:host github
;;                                                   :repo "jsalzbergedu/flycheck-google-cpplint"))
;;   :config
;;   (flycheck-add-next-checker 'lsp-ui 'c/c++-googlelint))

;; (use-package flycheck-clang-analyzer
;;   :demand t
;;   :after (flycheck lsp-ui)
;;   :straight t
;;   :config
;;   (flycheck-clang-analyzer-setup))
;; Unfortunatley I can't afford the performance hit from flycheck-clang-analyzer :(

(defun +cc-mode/compile ()
  (interactive)
  (require 'ccls)
  (with-temp-buffer
    (cd (projectile-project-root))
    (if (f-exists-p "CMakeLists.txt")
        (progn
          (cd "build")
          (compile "ninja"))
      (when (or (f-exists-p "Makefile") (f-exists-p "makefile"))
        (compile "make all")))))

(use-package json-pointer
  :defer t
  :straight (:type git
             :host github
             :repo "syohex/emacs-json-pointer"))

;; Candidates for running come from:
;; 1. Find all the files with int main
;; 2. Find those files in the compile commands
;; 3. Find what comes after -o
(defun +cc-mode/compute-executable (compile-command)
  (let* ((directory (cdr (assoc 'directory compile-command)))
         (arguments (cdr (assoc 'arguments compile-command)))
         (o-flag nil))
    (cl-loop for i from 0 below (length arguments)
             do (when (string= (aref arguments i) "-o")
                  (setq o-flag i)))
    (expand-file-name (aref arguments (+ 1 o-flag)) directory)))


(defun +cc-mode/choose ()
  (require 'cl-lib)
  (let ((full-file buffer-file-name)
        (file (f-filename buffer-file-name))
        (root (or (locate-dominating-file default-directory ".ccls-root")
                  (projectile-project-root))))
    (with-temp-buffer
      (cd root)
      (let* ((s (f-read "compile_commands.json"))
             (j (json-read-from-string s))
             (j (cl-loop for i from 0 below (length j)
                         collect (aref j i)))
             (j (cl-remove-if-not
                 (lambda (x) (string= (cdr (assoc 'file x)) file))
                 j))
             (executables (cl-loop for item in j
                                   collect
                                   (+cc-mode/compute-executable item)))
             (executable (completing-read "executable to run: "
                                          executables))
             (save (y-or-n-p "save this as the executable for this file? ")))
        (when save
          (let ((to-add `((:file ,full-file :executable ,executable))))
            (if (not (f-file-p ".cc-extras"))
                (f-write
                 (format "%s" to-add)
                 'utf-8
                 ".cc-extras")
              (let* ((plist (f-read ".cc-extras"))
                     (plist (read plist)))
                (f-write
                 (format "%s" (append to-add plist))
                 'utf-8
                 ".cc-extras")))))
        executable))))

(defun +cc-mode/run ()
  (interactive)
  (require 'f)
  (let* ((file buffer-file-name)
         (conf (expand-file-name ".cc-extras"
                                 (locate-dominating-file default-directory ".cc-extras")))
         (conf (and (f-file-p conf) conf))
         (conf (when conf (read (f-read conf 'utf-8))))
         (conf (when conf (cl-remove-if-not (lambda (x)
                                          (string= (plist-get x :file) file))
                                        conf)))
         (conf (when conf (car conf)))
         (executable (when conf
                       (plist-get conf :executable)))
         (executable (when executable
                       (format "%s" executable))))
    (when (not executable)
      (setq executable (+cc-mode/choose)))
    (message "Type of executable: %s" (type-of executable))
    (compile (compilation-read-command executable) t)))

  ;; (with-temp-buffer
  ;;   (locate-dominating-file default-directory ".cc-extras")
  ;;   (f-read )))
    ;; (call-process "rg"
    ;;               nil
    ;;               (current-buffer)
    ;;               nil
    ;;               "--glob"
    ;;               "*.c"
    ;;               "--json"
    ;;               "int main")
;;    (let* ((s (buffer-substring-no-properties 1 (point-max)))
;;           (j (json-read-from-string s))

;;           (path (json-pointer-get j "data/path/text")))
;;      (ansi-term
;;       (concat "./" (substring path 0 (- (length path) 2)))
;;       "*running program*"))))

(defun +cc-mode/test ()
  (interactive)
  (require 'ccls)
  (with-temp-buffer
    (cd (or (locate-dominating-file ".ccls-root")
            (projectile-project-root)))
    (if (f-exists-p "CMakeLists.txt")
        (progn (cd "build")
               (compile "ninja test"))
      (when (or (f-exists-p "Makefile") (f-exists-p "makefile"))
        (compile "make test")))))


;; (define-minor-mode cpp-project-mode
;;   "A mode which is activated whenever ccls-root can be found."
;;   :lighter ""
;;   :keymap (make-sparse-keymap))


;; (evil-define-key 'normal cpp-project-mode-map (kbd "SPC p") 'hydra-cpp/body)

;; (add-hook 'cpp-project-mode-hook 'evil-normalize-keymaps)

;; (define-globalized-minor-mode cpp-project-mode-global cpp-project-mode
;;   (lambda ()
;;     (when (locate-dominating-file default-directory ".ccls-root")
;;         (cpp-project-mode 1))))

;; (cpp-project-mode-global 1)

(use-package make-mode
  :straight nil
  :defer t
  :init
  (add-hook 'makefile-mode-hook 'prog-minor-modes-common))

(project-hydra hydra-c
  :test +cc-mode/test
  :compile +cc-mode/compile
  :stylecheck nil
  :search counsel-projectile-rg
  :git magit-status
  :run +cc-mode/run
  :and ("p" counsel-projectile-find-file)
  :and ("e" hydra-flycheck-error/body)
  :and ("a" lsp-ui-sideline-apply-code-actions)
  :and ("d" xref-find-definitions-other-window)
  :and ("f" xref-find-definitions))

(project-hydra hydra-cpp
  :test +cc-mode/test
  :compile +cc-mode/compile
  :stylecheck nil
  :search counsel-projectile-rg
  :git hydra-magit/body
  :run nil
  :and ("p" counsel-projectile-find-file)
  :and ("e" hydra-flycheck-error/body)
  :and ("a" lsp-ui-sideline-apply-code-actions)
  :and ("d" xref-find-definitions-other-window)
  :and ("f" xref-find-definitions))

(use-package cc-mode
  :defer t
  :after flycheck
  :straight nil
  :init
  ;; makefiles
  ;; c++ stuff
  (add-hook 'c++-mode-hook (lambda ()
                             (google-set-c-style)
                             (google-make-newline-indent)
                             (setq indent-tabs-mode nil
 				   tab-width 2
                                   c-basic-offset 2)
                             (prog-minor-modes-common)))
  (add-hook 'c++-mode-hook #'+ccls/enable-with-lens)
  (add-to-list 'auto-mode-alist '("conanfile\\.txt\\'" . conf-mode))
  (evil-define-key 'normal c++-mode-map (kbd "SPC p") 'hydra-cpp/body)
  (add-hook 'c++-mode-hook #'evil-normalize-keymaps)
  ;; c stuff
  (add-hook 'c-mode-hook (lambda ()
                           (google-set-c-style)
                           (google-make-newline-indent)
                           (setq indent-tabs-mode nil
                                 tab-width 2
                                 c-basic-offset 2)
                           (setq show-trailing-whitespace t)
                           (prog-minor-modes-common)))
  (add-hook 'c-mode-hook #'+ccls/enable-with-lens)
  (evil-define-key 'normal c-mode-map (kbd "SPC p") #'hydra-c/body))

;; Json
(use-package json-mode
  :defer t
  :straight (json-mode :type git
                       :host github
                       :repo "joshwnj/json-mode")
  :config (progn (add-hook 'json-mode-hook 'prog-minor-modes-common)))

;; Forth
(use-package forth-mode
  :straight (forth-mode :type git
                        :host github
                        :repo "larsbrinkhoff/forth-mode")
  :config
  (add-hook 'forth-mode-hook 'prog-minor-modes-common)
  (setq forth-executable "/usr/bin/gforth")
  :defer t)

(use-package forth-block-mode
  :straight nil
  :demand t
  :after forth-mode)

(use-package forth-interaction-mode
  :straight nil
  :demand t
  :after forth-mode)

;; Idris
(use-package idris-mode
  :defer t
  :straight (idris-mode :type git
                        :host github
                        :repo "idris-hackers/idris-mode")
  :config (add-prog-minor-modes-common 'idris-mode-hook
				       'idris-repl-mode-hook
				       'idris-ipkg-mode-hook))

;; Ruby
(use-package ruby-mode
  :defer t
  :straight (enh-ruby-mode :type git
                           :host github
                           :repo "jacott/Enhanced-Ruby-Mode")
  :init (progn (add-to-list 'auto-mode-alist
                              '("\\.rb\\'" . enh-ruby-mode))
                 (add-to-list 'auto-mode-alist
                              '("\\Rakefile\\'" . enh-ruby-mode)))
  :config (add-hook 'enh-ruby-mode-hook 'prog-minor-modes-common)
  :commands enh-ruby-mode)

(use-package inf-ruby
  :defer t
  :straight (inf-ruby :type git
                      :host github
                      :repo "nonsequitur/inf-ruby")
  :config (add-hook 'inf-ruby-mode-hook 'prog-minor-modes-common))

;; Xml
(use-package nxml-mode
  :defer t
  :straight nil
  :config (add-hook 'nxml-mode-hook 'prog-minor-modes-common))

;; Uml
(use-package plantuml-mode
  :defer t
  :straight (plantuml-mode :type git
                           :host github
                           :repo "skuro/plantuml-mode")
  :init
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

;; Coq
(use-package proof-general
  :straight t
  :defer t)

;; Haskell
(use-package haskell-mode
  :straight (haskell-mode :type git
                          :host github
                          :repo "haskell/haskell-mode")
  :init (setq haskell-process-type 'stack-ghci)
  :defer t)

(use-package inf-haskell
  :straight nil
  :demand t
  :after haskell-mode)

;; F*
(use-package fstar-mode
  :straight (fstar-mode :type git
                        :host github
                        :repo "FStarLang/fstar-mode.el")
  :defer t)

;; Unix config files
(use-package conf-mode
  :defer t
  :straight nil
  :init
  (add-hook 'conf-unix-mode-hook 'prog-minor-modes-common)
  (add-hook 'conf-javaprop-mode-hook 'prog-minor-modes-common))

;; CSV files
(use-package csv-mode
  :defer t
  :straight t
  :init
  (add-hook 'csv-mode-hook 'prog-minor-modes-common))

;; TODO: Build EmmyLua automatically
;; Lua mode
(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   (list
                    "/usr/bin/java"
                    "-cp"
                    (expand-file-name "EmmyLua-LS-all.jar" user-emacs-directory)
                    "com.tang.vscode.MainKt"))
                  :major-modes '(lua-mode)
                  :server-id 'emmy-lua))

(use-package lua-mode
  :defer t
  :straight t
  :hook ((lua-mode . prog-minor-modes-common)
         (lua-mode . lsp))
  :config
  (add-to-list 'company-backend 'company-lua))



;; Rosie Pattern language
(use-package rpl-mode
  ;; rpl doesnt play nice with lazy loading
  :demand t
  :straight (rpl-mode :type git
                      :host gitlab
                      :repo "rosie-pattern-language/rosie"
                      :files ("extra/emacs/rpl-mode.el"))
  :hook ((rpl-mode . prog-minor-modes-common))
  :config
  (setq *rpl-mode-verbose* nil))

;; Yaml
(use-package yaml-mode
  :defer t
  :straight t
  :hook ((yaml-mode . prog-minor-modes-common)))

(provide 'etoile-programming)
;;; etoile-programming.el ends here
