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
  :init (add-hook 'prog-minor-modes-common 'smartparens-mode)
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
        '(("[^&]\\(#\\(?!define\\)\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
           (1 (rainbow-colorize-itself 1)))
          ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)\\(?!define\\)+\\{1,4\\}\\)"
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

(provide 'etoile-programming)
;;; etoile-programming.el ends here
