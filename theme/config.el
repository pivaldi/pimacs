;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2024, Philippe Ivaldi <www.piprime.fr>
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation ; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; CODE:

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(unless (modulep! +no-font)
  (after!
    pimacs/default
    (let ((used-font
           (cond
            ((find-font (font-spec :name "Cascadia Code NF" ))
             (prog1
                 (font-spec :family "Cascadia Code NF" :height 120 :foreground "#DCDCCC")
               (load! "+ligature-cascadia")))
            ((find-font (font-spec :name "Source Code Pro"))
             (font-spec :family "Source Code Pro" :weight 'medium :size 15 :width 'normal :foreground "#DCDCCC"))
            ((find-font (font-spec :name "Terminus"))
             (font-spec :foundry "xos4" :family "Terminus" :weight 'medium :height 160 :width 'normal :foreground "#DCDCCC"))
            ((find-font (font-spec :name "Hack"))
             (font-spec :family "Hack" :weight 'medium :height 160 :width 'normal :foreground "#DCDCCC"))
            ((find-font (font-spec :name "DejaVu Sans Mono"))
             (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14 :foreground "#DCDCCC"))
            (t nil)
            )))
      (setq doom-font-increment 1
            doom-font used-font
            line-spacing 0)
      (set-frame-font doom-font nil t))
    ))


(unless (modulep! +no-zenburn-theme)
  ;; If you'd like to tweak the theme by changing just a few colors,
  ;; you can do so by defining new values in the
  ;; zenburn-override-colors-alist variable before loading the theme.
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg-05" . "#303030")))

  (use-package!
      zenburn-theme
    :init
    (setq
     ;; use variable-pitch fonts for some headings and titles
     zenburn-use-variable-pitch t
     ;; scale headings in org-mode
     zenburn-scale-org-headlines t
     ;; scale headings in outline-mode
     zenburn-scale-outline-headlines t
     zenburn-fg "#dcdccc"
     )

    :config
    (load-theme 'zenburn t)
    (set-face-attribute 'completions-annotations nil :foreground "#A9A999"))

  (set-face-attribute 'font-lock-comment-face nil :foreground "#8FAF8F")
  (set-face-attribute 'region nil :underline "#459090" :background "#000000")
  ;; (set-face-attribute 'show-paren-mismatch nil :background "#FF3333" :underline "#FFFF33" :foreground "#6F6F6F")

  (after! popup
    (set-face-attribute
     'popup-menu-selection-face nil
     :background (face-attribute 'popup-menu-mouse-face :background)
     :foreground (face-attribute 'popup-menu-mouse-face :foreground))
    (set-face-attribute
     'popup-tip-face nil
     :background "#555555"
     :foreground "#bfbbbf"
     :box nil
     )
    (set-face-attribute 'popup-menu-face nil :inherit 'default :background "#000"))

  (after! avy
    (set-face-attribute 'avy-lead-face nil :foreground "#93E0E3" :background "#50A0A0")
    (set-face-attribute 'avy-lead-face-0 nil :foreground "#93E0E3")
    (set-face-attribute 'avy-lead-face-1 nil :foreground "#93E0E3")
    (set-face-attribute 'avy-lead-face-2 nil :foreground "#93E0E3")
    (set-face-attribute 'avy-background-face nil :foreground "#A5A595"))

  (after! bm
    (set-face-attribute 'bm-fringe-persistent-face nil :background (face-attribute 'fringe :background)  :foreground "#FF3333")
    (set-face-attribute 'bm-fringe-face nil :background (face-attribute 'fringe :background)  :foreground "#33FFFF")
    (set-face-attribute 'bm-face nil :background nil  :underline "#8FAF8F" :foreground "unspecified")
    (set-face-attribute 'bm-persistent-face nil :background nil :underline "#DFAF8F" :foreground "unspecified"))

  (after! rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-base-error-face nil :background "unspecified" :foreground "#FF5555")))



(setq-default
 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
 display-line-numbers nil
 )

;; Disable hl-line-mode in prog-mode and text-mode.
(use-package!
    hl-line
  :defer t
  :config
  (setq global-hl-line-modes '(special-mode org-agenda-mode dired-mode))
  (set-face-attribute 'hl-line  nil :background "#000000")
  )

(unless (modulep! +no-whitespace-style)
  ;; See useless white-spaces
  (set-face-attribute 'trailing-whitespace nil
                      :background "#4F6666")
  (set-face-attribute 'whitespace-trailing nil :background "#4F6666")
  (set-face-attribute 'whitespace-tab nil :background "#333333")
  (setq whitespace-style '(face tabs trailing)))

;; ------------------
;; * Comment header *
(defface pim-comment-section-face
  `((t
     ( :foreground "yellow")))
  "Face used to highlighting header of comment section."
  :group 'faces)
(defface pim-comment-sub-section-face
  `((t
     ( :foreground "white")))
  "Face used to highlighting header of comment section."
  :group 'faces)

(provide 'pimacs/theme)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/theme/config.el")
;; End:
