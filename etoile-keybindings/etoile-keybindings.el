;;; etoile-keybindings.el --- Keybindings for etoile

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/etoile-keybindings
;; Version: 0.1.0
;; Keywords: keybindings etoile

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
(use-package undo-tree
  :demand t
  :straight t
  :config
  (global-undo-tree-mode 1))

(use-package general
  :demand t
  :straight (general :no-native-compile t)
  :init
  (eval-and-compile
    (require 'general)))

(setq evil-want-keybinding nil)

(use-package evil
  :demand t
  :straight t
  :after (evil-collection)
  :config
  (evil-mode 1)
  (global-undo-tree-mode)
  :general
  (:keymaps '(normal motion)
            "SPC" nil
            "SPC l" 'windmove-cf-right
            "SPC h" 'windmove-cf-left
            "SPC k" 'windmove-cf-up
            "SPC j" 'windmove-cf-down
            "SPC q" 'kill-this-buffer
            "SPC a" 'switch-to-buffer
            "SPC SPC" 'shell-pop
            "C-s" 'swiper))

(use-package evil-surround
  :straight t
  :demand t
  :after evil
  :config
  (push '(?\( . ("(" . ")")) evil-surround-pairs-alist)
  (push '(?\[ . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?\{ . ("{" . "}")) evil-surround-pairs-alist)
  (global-evil-surround-mode 1)
  :general
  (:keymaps '(evil-surround-mode-map) :states '(visual)
            "S" 'evil-surround-region))

;; TODO move this to a + package
;; Either move across emacs windows or stumpwm windows
(defun windmove-plain (dir)
  "Moves window focus in direction DIR"
  (interactive "sDIR: ")
  (cond
   ((string= "up" dir) (windmove-up))
   ((string= "down" dir) (windmove-down))
   ((string= "left" dir) (windmove-left))
   ((windmove-right) t)
   (t nil)))

(defun check-stumpish ()
  (= (call-process "which" nil nil nil "stumpish") 0))

(defun stump-move (dir)
  "Move stumpwm focus in direction DIR"
  (interactive "sDIR: ")
  (when (check-stumpish)
    (make-process :name "windmove" :buffer nil :command
                  (list "stumpish"
                        (concat "move-focus " dir)))))


(defun windmove-or-change-focus (dir)
  "Windmoves or moves stumpwms focus in direction DIR"
  (interactive "sDIR: ")
  (unless (ignore-errors (windmove-plain dir))
    (stump-move dir)))

(defun windmove-cf-right ()
  (interactive)
  "Move or window or stumpwm focus right"
  (windmove-or-change-focus "right"))

(defun windmove-cf-left ()
  (interactive)
  "Move or window or stumpwm focus left"
  (windmove-or-change-focus "left"))

(defun windmove-cf-up ()
  (interactive)
  "Move or window or stumpwm focus up"
  (windmove-or-change-focus "up"))

(defun windmove-cf-down ()
  (interactive)
  "Move or window or stumpwm focus down"
  (windmove-or-change-focus "down"))

(use-package evil-collection
  :demand t
  :straight t
  :init
  :config
  (setq evil-collection-mode-list (remove 'company evil-collection-mode-list))
  (setq evil-collection-mode-list (remove 'slime evil-collection-mode-list))
  (push "SPC" evil-collection-key-blacklist)
  (evil-collection-init))

;; Unset space in many packages
(use-package dired
  :defer t
  :straight nil
  :general
  (:keymaps '(dired-mode-map)
            "SPC" nil))


(use-package help-mode
  :defer t
  :straight nil
  :general
  (:keymaps '(help-mode-map)
            "SPC" nil))

(use-package info
  :defer t
  :straight nil
  ;; For some reason info must be brute forced here
  :config
  (substitute-key-definition 'Info-scroll-up nil Info-mode-map)
  (substitute-key-definition 'Info-help 'evil-backward-char Info-mode-map)
  (substitute-key-definition 'Info-history-back 'evil-forward-char Info-mode-map))
  ;; (evil-define-key 'normal 'Info-mode-map (kbd "p") 'Info-prev))
;; 
;; ;; ;; Ivy, Swiper, Counil, Flx, and Amx
;; flx native compile breaks ivy
(use-package flx
  :straight t
  ;; (flx :no-native-compile t)
  :demand t)

(use-package swiper
  :straight t
  :general
  ("C-s" 'swiper))

(use-package ivy
  :demand t
  :straight t
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  :general
  ("C-c C-r" 'ivy-resume)
  :commands ivy-mode)

(use-package hydra
  :straight t
  :demand t)

(use-package ivy-hydra
  :straight t
  :demand t)

(use-package amx
  :straight t
  :defer t)

(use-package counsel
  :demand t
  :straight t
  :general
  (:keymaps '(normal motion)
            "SPC f" 'counsel-find-file)
  ("<menu>" 'ignore)
  ("M-x" 'counsel-M-x)
  ("C-x C-f" 'counsel-find-file))

(provide 'etoile-keybindings)
;;; etoile-keybindings.el ends here
