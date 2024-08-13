;;; Package pimacs/keys --- PIMacs key binding -*- lexical-binding: t; -*-
;; Copyright (c) 2024, Philippe Ivaldi <www.piprime.fr>

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

;;; Commentary:

;;; Code:

(map! :desc "If at end of line, join with following; otherwise kill line. #pim" "C-k" #'pim/kill-and-join-forward)

;; ;; TODO : Use the package easy-kill
(map! :desc "filename in the minibuffer, in the buffer with C-u" "<f8>. #pim" #'pim/buffer-file-name)
(map! :desc "echo filename in the minibuffer and put in the kill ring. #pim" "<S-f8>"
      (lambda nil
        (interactive)
        (pim/buffer-file-name nil t)))

(map! :desc "Delete characters backward until encountering the beginning of a word. #pim"
      "<C-backspace>" #'pim/backward-delete-word)

(map! "<M-backspace>" #'pim/backward-delete-sexp)
(map! "<M-delete>"
      (lambda ()
        (interactive)
        (beginning-of-line)
        (kill-line)))

;; ------------------------
;; * Key for other-window *
(map!
 :desc "Select another window in cyclic ordering of windows or with `ace-window`. #pim" "<C-next>"
 (lambda (arg)
   (interactive "p")
   (if (functionp 'ace-window)
       (ace-window arg)
     (other-window 1 nil))))
(map!
 :desc "Select another window in backwards ordering of windows or with `ace-window`. #pim" "<C-prior>"
 (lambda (arg)
   (interactive "p")
   (if (functionp 'ace-window)
       (ace-window arg)
     (other-window -1 nil))))

;; -------------------------------------------
;; * Filename completion anywhere with S-Tab *
(autoload 'comint-dynamic-complete-filename "comint" "" t)
;; List of suffixes to neglect during completion
(setq comint-completion-fignore (quote ("{" "}" "(" ")" "$" "=" "\"" "`" "'" "[" "]" "%" "<" ">")))
(map! :desc "Dynamically complete the filename under the cursor. #pim"
      "<S-iso-lefttab>" #'comint-dynamic-complete-filename
      "<S-tab>" #'comint-dynamic-complete-filename)

(map! (:after consult)
      :desc "Open recent file à la Chromium/Firefox. #pim"
      :g "C-S-t" 'consult-recent-file)

(cond
 ((modulep! :completion vertico)
  (map!
   :desc "Swith to buffer with the same workspace. #pim"
   "<C-tab>" #'+vertico/switch-workspace-buffer)
  )

 ((modulep! :completion helm)
  (map!
   :desc "Swith to buffer with the same workspace. #pim"
   "<C-tab>" #'+helm/workspace-mini)
  )
 ((modulep! :completion ivy)
  (map!
   :desc "Swith to buffer with the same workspace. Use 'C-x B' for extended buffer/file list. #pim"
   "<C-tab>" #'+ivy/switch-workspace-buffer)
  )
 )

(when (modulep! :ui workspaces)
  ;; (map!
  ;;  :desc "Switch to workspace. #pim"
  ;;  "<s-tab>" #'+workspace/switch-to)
  (map! :leader
        :prefix ("w" . "workspaces/windows")
        :desc "Load a workspace. #pim" "L" #'+workspace/load)

  (map! :leader
        :prefix ("TAB" . "switch")
        :desc "Switch to last workspace. #pim"  "TAB"   #'+workspace/other
        :desc "Switch to workspace. #pim"  "w"   #'+workspace/switch-to
        )
  )

;; ;; TODO : enable it
;; ;; ----------------------
;; ;; * Keys for info-mode *
;; (add-hook 'Info-mode
;;           (lambda ()
;;             (define-key Info-mode-map (kbd "<") 'Info-history-back)
;;             (define-key Info-mode-map (kbd ">") 'Info-history-forward)))

(map! :desc "Browse url at point. #pim" "C-c b" #'browse-url-at-point)

(map! :desc "Delete current window and buffer. #pim" "<f12>" 'pim/kill-window-and-buffer)

(map! :desc "Delete current window and buffer. #pim" "<C-S-iso-lefttab>" 'pim/indent-whole-buffer)

;; ;; Todo : enable this !
;; (if (require 'move-text nil t)
;;     (move-text-default-bindings)
;;   (progn
;;     (defun move-line-up (&optional n)
;;       "Moves current line up leaving point in place.  With a prefix
;; argument, moves up N lines."
;;       (interactive "p")
;;       (if (null n) (setq n 1))
;;       (let ((col (current-column)))
;;         (beginning-of-line)
;;         (next-line 1)
;;         (transpose-lines (- n))
;;         (previous-line 2)
;;         (forward-char col)))
;;     (global-set-key (kbd "<M-up>") 'move-line-up)

;;     (defun move-line-down (&optional n)
;;       "Moves current line down leaving point in place.  With a prefix
;; argument, moves down N lines."
;;       (interactive "p")
;;       (if (null n) (setq n 1))
;;       (let ((col (current-column)))
;;         (beginning-of-line)
;;         (next-line 1)
;;         (transpose-lines  n)
;;         (previous-line 1)
;;         (forward-char col)))
;;     (global-set-key [(meta down)] 'move-line-down)
;;     )
;;   )

(map! :desc "Find file as root. #pim" "C-x C-r" #'pim/find-file-root)

(map! :desc "* Move cursor at beginning of line or first non blank character. #pim" "<home>" #'pim/home)
(map! :desc "* Move cursor at beginning of line or first non blank character. #pim" "C-M-<prior>" #'pim/home)
(map! :desc "* Move cursor at end of line. #pim" "C-M-<next>" 'end-of-line)

(map! :desc "Use fill line or region as auto-fill-mode does. #pim" "M-q" #'pim/fill)

(map! :desc "Comment/Uncomment the entire line and indent. #pim" "C-%"
      (lambda nil
        (interactive)
        (pim/?comment t)))
(map! :desc "Comment/Uncomment the entire line but not indent. #pim" "C-ù" #'pim/?comment)

;; ;; Semicolon and comma at the end of the line
(let ((keysm (kbd "C-;"))
      (keyco (kbd "C-,")))
  (map! :desc "Fancy insert/delete semicolon at the end of the line. #pim" keysm #'pim/insert-semicol-at-end-of-line)
  (map! :desc "Fancy insert/delete comma at the end of the line. #pim" keyco #'pim/insert-comma-at-end-of-line)
  ;; Rebind flyspell default key-binding
  (after! flyspell
          (define-key flyspell-mode-map
                      keysm 'pim/insert-semicol-at-end-of-line)
          (define-key flyspell-mode-map
                      keyco 'pim/insert-comma-at-end-of-line)))

(map! :desc "Insert a section comments. #pim" "C-Μ" #'pim/insert-comment-section)
(map! :desc "Insert a section comments. #pim" "C-*" #'pim/insert-comment-sub-section)


;; ;; TODO : Is it needed ?
;; ;; --------------------------------------------------------
;; ;; * Seeking a makefile recursively in directories higher *
;; (setq pim-compilation-filenames '("Makefile" "makefile"))

;; (defun pim-get-nearest-compilation-dir ()
;;   "Search for the compilation file traversing up the directory tree. Return the directory, not the file !
;; Src : http://www.emacswiki.org/cgi-bin/wiki/UsingMakefileFromParentDirectory"
;;   (let ((dir default-directory)
;; 	(parent-dir (file-name-directory (directory-file-name default-directory)))
;; 	(nearest-compilation-dir 'nil))
;;     (while (and (not (string= dir parent-dir))
;; 		(not nearest-compilation-dir))
;;       (dolist (filename pim-compilation-filenames)
;; 	(setq file-path (concat dir filename))
;; 	(when (file-readable-p file-path)
;; 	  (setq nearest-compilation-dir dir)))
;;       (setq dir parent-dir
;; 	    parent-dir (file-name-directory (directory-file-name parent-dir))))
;;     nearest-compilation-dir))

;; (defun pim-compile-above-makefile ()
;;   (interactive)
;;   (let* ((mkf (pim-get-nearest-compilation-dir))
;;          (default-directory (directory-file-name mkf)))
;;     (if default-directory
;;         (progn
;;           (cd default-directory)
;;           (compile "[ -e ./ovyaproject.rc ] && source ovyaproject.rc; make")))))
;; (global-set-key (kbd "<f9>") 'pim-compile-above-makefile)


;; ;; TODO : to be implemented
;; ;; -----------------------------------------------
;; ;; * Scroll down the page rather than the cursor *
;; ;; use the key "Scroll Lock Num" ("Num Défil" in french) to toggle.
;; ;; C-up et C-down
;; (autoload 'pim-scroll-lock-mode "pim-scroll-lock" "Toggle pim-scroll-lock-mode." t)
;; (global-set-key (kbd "<Scroll_Lock>") 'pim-scroll-lock-mode)
;; ;; Switches to hl-line-mode when the cursor is locked.
;; (setq pim-scroll-hl t)


;; C-/ is undo by default
(map! :desc "Redo from undo-fu. #pim" "C-z" #'undo-fu-only-undo)
(map! :desc "Undo from undo-fu. #pim" "C-S-z" #'undo-fu-only-redo)

;; Non-breaking spaces with quotes please.
(map! :desc "Non-breaking spaces with quotes please. #pim" "«"
      (lambda nil
        (interactive)
        (insert
         "« ")(insert
         " »")(backward-char
         2)))
(map! :desc "Non-breaking spaces with quotes please. #pim" "»" (lambda nil (interactive) (insert " »")))


;; ;; TODO : to be enabled
;; ;; --------------------------------
;; ;; * Highlight the current column *
;; (when (locate-library "column-marker")
;;   (autoload 'column-marker-1 "column-marker" "Highlight a column." t)
;;   ;; http://www.emacswiki.org/cgi-bin/wiki/col-highlight.el
;;   (require 'col-highlight)
;;   ;; Raccourci sur [f10]
;;   (global-set-key (kbd "<f10>") 'column-highlight-mode))

(use-package!
 jumpc
 ;; :defer t ;; does not work when deferred
 :config
 (jumpc)
 (map! :desc "Jump to prev pos. #pim" "C-<" #'jumpc-jump-backward)
 (map! :desc "Jump to next pos. #pim" "C->" #'jumpc-jump-forward))

;; TODO : To be implemented
;; ;; Define C-x up | C-x down | C-x right | C-x left to resize the windows
;; (require 'pim-resize-window "pim-resize-window.el" t)

;; ----------------------
;; * disable insert key *
(map! :desc "Disable overwrite-mode pressing <insert> key. #pim" "<insert>"
      (lambda nil
        (interactive)
        (message "Insert is desabled by PIMacs. Use \"M-x overwrite-mode\" instead.")))

;; ----------------------------
;; * C-f1 toggle the menu bar *
(map! :desc "Toggle menu bar. #pim" "<C-f1>" #'menu-bar-mode)

;; ----------------------------------
;; * Useful binding to create macro *
(map! :desc "Start the definition of a macro. #pim" "S-<f4>" #'kmacro-start-macro)
(map! :desc "Ending the definition of a macro. #pim" "<f4>" #'kmacro-end-or-call-macro)
(map! :desc "Edit the last defined macro. #pim" "<C-f4>" #'kmacro-edit-macro)

;; ------------------------
;; * Expand M-g goto-xxx *
(map! :desc "Move backward to the beginning of a defun. #pim" "M-g d" #'beginning-of-defun)

;; -------------------
;; * scroll in place *
(map! :desc "Scroll up keeping the cursor on the same line. #pim" "<C-M-up>"
      (lambda nil
        (interactive)
        (let ((scroll-preserve-screen-position t))
          (scroll-up 1))))

(map! :desc "Scroll down keeping the cursor on the same line. #pim" "<C-M-down>"
      (lambda nil
        (interactive)
        (let ((scroll-preserve-screen-position t))
          (scroll-down 1))))

;; -------------------------
;; * Navigate in long line *
(map! :desc "Scrolling up long line. #pim" "<C-up>"
      (lambda nil
        (interactive)
        (let ((line-move-visual nil))
          (forward-line -1))))

(map! :desc "Scrolling down in long line. #pim" "<C-down>"
      (lambda nil
        (interactive)
        (let ((line-move-visual nil))
          (forward-line))))


(map! :leader
      :prefix ("8" . "utf-8")
      (:prefix ("1" . "fraction one")
               :desc "Fraction one half #pim" "2" "½"
               :desc "Fraction one third #pim" "3" "⅓"
               :desc "Fraction one quarter #pim" "4" "¼"
               :desc "Fraction one fifth #pim" "5" "⅕"
               :desc "Fraction one sixth #pim" "6" "⅙"
               :desc "Fraction one seventh #pim" "7" "⅐"
               :desc "Fraction one eighth #pim" "8" "⅛"
               :desc "Fraction one ninth #pim" "9" "⅑"
               :desc "Fraction one tenth #pim" "0" "⅒"
               )

      (:prefix ("f" . "face")
               :desc "Slightly Smiling Face 🙂 #pim" "s" "🙂"
               :desc "Grinning Face 😀 #pim" "g" "😀"
               :desc "Grimacing Face 😬 #pim" "G" "😬"
               :desc "Unamused Face 😒 #pim" "u" "😒"
               :desc "Disappointed Face 😞 #pim" "d" "😞"
               :desc "Confounded Face 😖 #pim" "c" "😖"
               :desc "Winking Face 😉 #pim" "w" "😉"
               :desc "Face with Tears of Joy 😂 #pim" "j" "😂"
               :desc "Worried Face 😟 #pim" "W" "😟"
               :desc "Smiling Face Cold Sweat 😅 #pim" "C" "😅"
               :desc "Face Screaming In Fear 😱 #pim" "S" "😱"
               :desc "Smiling Face Closed Eyes 😆 #pim" "e" "😆"
               :desc "Loudly Crying Face 😭 #pim" "l" "😭"
               )

      (:prefix ("s" . "symbol")
               :desc "Warning Sign ⚠️ #pim" "w" "⚠️"
               :desc "Zap Sign ⚡ #pim" "z" "⚡"
               )

      (:prefix ("a" . "arrow")
               :desc "Upwards Arrow with Equilateral Arrowhead 🠕 #pim" "u" "🠕"
               :desc "Rightwards Arrow with Equilateral Arrowhead 🠖 #pim" "r" "🠖"
               :desc "Leftwards Arrow with Equilateral Arrowhead 🠔 #pim" "l" "🠔"
               :desc "Downwards Arrow with Equilateral Arrowhead 🠗 #pim" "d" "🠗"

               :desc "Heavy Upwards Arrow with Equilateral Arrowhead 🠙 #pim" "U" "🠙"
               :desc "Heavy Rightwards Arrow with Equilateral Arrowhead 🠚 #pim" "R" "🠚"
               :desc "Heavy Leftwards Arrow with Equilateral Arrowhead 🠘 #pim" "L" "🠘"
               :desc "Heavy Downwards Arrow with Equilateral Arrowhead 🠛 #pim" "D" "🠛"
               )
      )

(map! :desc "Switch to the next user buffer. #pim" "<mouse-9>" #'pim/next-user-buffer)
(map! :desc "Switch to the previous user buffer. #pim" "<mouse-8>" #'pim/previous-user-buffer)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/keys/+emacs.el")
;; End:
