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
    (if
        (find-font (font-spec :name "TerminessTTF NF"))
        (progn
          (setq
           doom-font-increment 1
           ;; TODO : Make the default font size an option
           doom-font (font-spec :family "TerminessTTF NF" :foundry "PfEd" :weight 'bold :size 20 :foreground "#DCDCCC")
           )
          (set-frame-font doom-font nil t))

      (add-to-list 'pim-error-msgs "Please install Terminess TTF Nerd Fonts : https://github.com/ryanoasis/nerd-fonts"))
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

  (after! avy
    (set-face-attribute 'avy-lead-face nil :foreground "#93E0E3" :background "#50A0A0")
    (set-face-attribute 'avy-lead-face-0 nil :foreground "#93E0E3" :background "#50A0A0")
    (set-face-attribute 'avy-lead-face-1 nil :foreground "#93E0E3" :background "#50A0A0")
    (set-face-attribute 'avy-lead-face-2 nil :foreground "#93E0E3" :background "#50A0A0")))


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
  )

(unless (modulep! +no-whitespace-style)
  ;; See useless white-spaces
  (set-face-attribute 'trailing-whitespace nil
                      :background "#2F5555")
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
