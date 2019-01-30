;;; etoile-themes.el --- Theme settings for etoile-emacs -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/etoile-themes
;; Version: 0.1.0
;; Keywords: themes etoile emacs

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
;; Theme settings for etoile-emacs

;;; Code:
(message "proof of concept")

;; Text editing
(set-face-attribute 'fixed-pitch nil
                    :family "Inconsolata")

(set-face-attribute 'default nil
                    :height 113
                    :family "Inconsolata"
                    :foundry "PfEd")
(add-to-list 'default-frame-alist '(font-backend "xft"))

;; Fix box building characters
(defun fix-box-building (&optional frame)
  "Sets a fallback font to make the box-building characters look right"
  (set-fontset-font "fontset-default" '(#x2502 . #x2502) "Fira Mono" frame)
  (redisplay t))
(add-hook 'after-make-frame-functions 'fix-box-building)
(add-hook 'window-setup-hook 'fix-box-building)


;; Mode line
(setq display-battery-mode t)

;; Emojis
(use-package emojify
  :straight (emojify :type git
                     :host github
                     :repo "iqbalansari/emacs-emojify"
                     :files ("data" "emojify.el"))
  :files ("data" "emojify.el")
  :demand t
  :init (setq emojify-emoji-styles '(unicode))
  :config (global-emojify-mode))

;; Mode line
(use-package rich-minority
  :straight (rich-minority :type git
                           :host github
                           :repo "Malabarba/rich-minority")
  :demand t)

(use-package smart-mode-line
  :straight (smart-mode-line :type git
                             :host github
                             :repo "Malabarba/smart-mode-line")
  :demand t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'light)
  (display-time-mode 1))

;; Use nice looking colors
(use-package flatui-theme
  :straight (flatui-theme :type git
                          :host github
                          :repo "john2x/flatui-theme.el")
  :demand t
  :config (load-theme 'flatui t))

;; TODO: MOVE TO LSP-UI
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

;; TODO: MOVE TO CCLS
(setq ccls-sem-highlight-method 'overlay t)
(set-face-attribute 'ccls-code-lens-face nil
                    :inherit 'shadow
                    :height 0.8)


(provide 'etoile-themes)
;;; etoile-themes.el ends here
