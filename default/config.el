;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2024, Philippe Ivaldi <www.piprim.net>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:

;; Set general parameters of Emacs for pimacs users

;;; Code:

(setq-default
 ;; Fix missing warning-suppress-types function
 warning-suppress-types nil
 ;; Draw block cursor as wide as the glyph under it
 x-stretch-cursor t
 ;; Title Format
 frame-title-format (list user-real-login-name "@" system-name " : %S" 'default-directory "%b")
 inhibit-startup-message t
 ;; Replace the bell by visible blink
 visible-bell t
 ;; I don't want graphic dialog.
 use-dialog-box nil
 use-file-dialog nil
 tab-width 4
 buffers-menu-max-size nil
 history-length 200
 scroll-step 1
 scroll-conservatively 10000
 ;; Increase the number of macro recursions
 ;; We can also set it to 9999 but then there may be stack overflows.
 max-lisp-eval-depth 5000
 max-specpdl-size max-lisp-eval-depth
 ;;Ignore the case in completion mode.
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;;Highlight the region when the mark is active.
 transient-mark-mode t
 ;; Save everything before compiling
 compilation-ask-about-save nil
 ;; Size of the compilation window.
 compilation-window-height 12
 ;; Non-nil to scroll the *compilation* buffer window as output appears.
 compilation-scroll-output t
 ;; Allow local variables
 enable-local-variables t
 ;; Controls if scroll commands move point to keep its screen line unchanged.
 scroll-preserve-screen-position t
 ;; Number of lines of margin at the top and bottom of a window.
 ;; Pose des problèmes avec la version de gnus que j'utilise actuellement...
 ;; rajouter (set (make-local-variable 'scroll-margin 0)) dans le hook de `gnus-summary-mode-hook'
 scroll-margin 4
 ;; Maximum number of lines to keep in the message log buffer.
 message-log-max 1000
 ;; `apropos' search all
 apropos-do-all t ;; Key binding "C-h a".
 ;; Fill bulleted and indented lines.
 adaptive-fill-mode t
 ;; Do not add a new string to `kill-ring' when it is the same as the last one.
 kill-do-not-save-duplicates t
 ;; Preferred split window horizontally
 split-width-threshold most-positive-fixnum
 ;; Emacs will initiate GC every 20MB allocated because we have a modern machine
 gc-cons-threshold 20000000
 gc-cons-percentage 1
 ;; Show all process with M-x proced
 ;; https://www.masteringemacs.org/article/displaying-interacting-processes-proced
 proced-filter 'all
 show-trailing-whitespace t
 show-leading-whitespace t
 nobreak-char-display t
 ;; Permanently force Emacs to indent with spaces, never with TABs:
 indent-tabs-mode nil
 ;; Maximum coloration
 font-lock-maximum-decoration t
 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
 display-line-numbers-type nil
 package-native-compile t
 ;; Configure the fill column indicator.
 display-fill-column-indicator-character "│"
 display-fill-column-indicator t
 display-fill-column-indicator-column t
 ;; Enable matching whitespace literally.
 search-whitespace-regexp nil
 )

;; ---------------------
;; * Prefered encoding *
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")


;; See the name of the current function in the mode line.
(after! which-func
  (which-function-mode 1))

(if (executable-find "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H -i --hidden --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 39))
  (progn
    (add-to-list 'pim-error-msgs "Please install rg : https://github.com/BurntSushi/ripgrep")
    (grep-apply-setting
     'grep-find-command
     '("find . -type f ! -regex '.*/node_modules/.*' ! -regexp '.*/dist/.*' ! -regex '.*/vendor/.*' ! -regex '.*\\.git/.*' -exec grep -Hni '' {} \\;" . 132))))

;; ----------------------------------------
;; * HTML rendering for buffers and files *
;; Just call `htmlize-view-buffer' to show the current buffer in
;; your web browser.
;;;###package html-view
(use-package!
    htmlize-view
  :defer t
  ;; :commands (htmlize-buffer htmlize-file htmlize-many-files
  ;;                           htmlize-many-files-dired
  ;;                           htmlize-region htmlize-view-buffer)
  :config
  (setq
   htmlize-convert-nonascii-to-entities nil
   htmlize-html-charset "utf-8")
  (htmlize-view-add-to-files-menu))

;; Does not work actually
;; (use-package!
;;     consult
;;   :defer t
;;   :config
;;   ;; Preview immediately on C-SPC, on up/down after 1s, on any other key after 2s
;;   (consult-customize consult-theme
;;                      :preview-key
;;                      '("C-SPC"
;;                        :debounce 1 "<up>" "<down>"
;;                        :debounce 2 any)))

(after! which-key
  :config
  (setq which-key-max-description-length 50)
  (setq max-mini-window-height 0.33)
  )

(load! "+doom")
(load! "+hooks")
(load! "+modes")

(provide 'pimacs/default)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/config.el")
;; End:
