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

;; Cleaning up of unneccesary visual elements
(set-fringe-mode (setq fringe-mode 0))
(setq fringes-outside-margins t) ; for when they're necessary
(set-scroll-bar-mode (setq scroll-bar-mode nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq cursor-in-non-selected-windows nil)

;; Text editing
(set-face-attribute 'fixed-pitch nil
                    :font "Inconsolata LGC")

(set-face-attribute 'fixed-pitch-serif nil
                    :font "League Mono")

(set-face-attribute 'default nil
                    :height 120
                    :family "Inconsolata LGC")

(defface etoile-themes-linum-fira '((t :inherit linum :family "Fira Mono"))
  "Face for displaying characters in fira mono")

(add-to-list 'default-frame-alist '(font-backend "xft"))

(use-package nlinum
  :straight t
  :demand t
  :config
  (setq nlinum-format "%4d │ ")
  (setq nlinum-format-function
        (lambda (line width)
          (let* ((is-current-line (= line nlinum--current-line))
                 (str (format nlinum-format line)))
            (when (< (length str) width)
              ;; Left pad to try and right-align the line-numbers.
              (setq str (concat (make-string (- width (length str)) ?\ ) str)))
            (put-text-property 0 (- width 2) 'face
                               'linum
                               str)
	    (put-text-property (- width 2) (- width 1) 'face 'etoile-themes-linum-fira str)
	    (put-text-property (- width 1) width 'face 'linum str)
            ; (setq str (concat str " "
            ;                   (propertize "│" 'face 'etoile-themes-linum-fira)
            ;                   " "))
            str))))

;; (use-package nlinum-hl
;;   :straight (emacs-nlinum-hl :type git
;;                              :host github
;;                              :repo "hlissner/emacs-nlinum-hl")
;;   :after nlinum
;;   :demand t)

;; Fix box building characters
;; (defun fix-box-building (&optional frame)
;;   "Sets a fallback font to make the box-building characters look right"
;;   (set-fontset-font "fontset-default" '(#x2502 . #x2502) "Fira Mono" frame)
;;   (redisplay t))
;; (add-hook 'after-make-frame-functions 'fix-box-building)
;; (add-hook 'window-setup-hook 'fix-box-building)

;; (set-fontset-font "-PfEd-Inconsolata LGC-normal-normal-normal-*-16-*-*-*-m-0-fontset-auto2" '(#x2502 . #x2502) "Fira Mono" (car (frame-list)))

;; Mode line (setq display-battery-mode t)
;; Emojis
(use-package emojify
  :straight (emojify :type git
                     :host github
                     :repo "iqbalansari/emacs-emojify"
                     :files ("data" "emojify.el"))
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
;; (use-package flatui-theme
;;   :straight (flatui-theme :type git
;;                           :host github
;;                           :repo "john2x/flatui-theme.el")
;;   :demand t
;;   :config (load-theme 'flatui t))

(use-package all-the-icons
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :demand t
  :config
  (load-theme 'doom-one-light t)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'normal)
  (set-face-attribute 'font-lock-doc-face nil
                      :slant 'italic)
  (doom-themes-treemacs-config))

;; TODO: MOVE TO LSP-UI
;; (set-face-attribute 'lsp-ui-sideline-code-action nil
;;                     :foreground "#2ecc71")

;; (set-face-attribute 'lsp-ui-sideline-current-symbol nil
;;                     :foreground "white"
;;                     :box '(:line-width -1 :color "white")
;;                     :weight 'ultra-bold
;;                     :height 0.95)

;; (set-face-attribute 'lsp-ui-sideline-symbol nil
;;                     :foreground "#7f8c8d"
;;                     :box '(:line-width -1 :color "#7f8c8d")
;;                     :height 0.9)

;; TODO: MOVE TO CCLS
(setq ccls-sem-highlight-method 'overlay)
;; (set-face-attribute 'ccls-code-lens-face nil
;;                     :inherit 'shadow
;;                     :height 0.8)


;; TODO: MOVE TO ITS OWN PACKAGE
;; borrowed from with-emacs
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)

(setq blink-matching-paren 'show)

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(cl-defstruct etoile-themes-overlay-remover
  fn)

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
    (lambda (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp ov)
        (delete-overlay ov))
      ;; check if it's appropriate to show match info,
      ;; see `blink-paren-post-self-insert-function'
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; rebind `minibuffer-message' called by
        ;; `blink-matching-open' to handle the overlay display
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq ov (display-line-overlay+
                                 (window-start) msg ))))))
          (blink-matching-open)
          (let ((remover (make-etoile-themes-overlay-remover)))
            (setf (etoile-themes-overlay-remover-fn remover)
                  (lambda ()
                    (when (overlayp ov) (delete-overlay ov))
                    (remove-hook 'post-command-hook (etoile-themes-overlay-remover-fn remover) :local)))
            (add-hook 'post-command-hook (etoile-themes-overlay-remover-fn remover) nil :local)))))))

(setq show-paren-style 'paren
      show-paren-delay 0.03
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren nil
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(provide 'etoile-themes)
;;; etoile-themes.el ends here
