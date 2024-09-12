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
(use-package! ace-window
  :preface
  (map!
   :desc "Select another window in cyclic ordering of windows (with `ace-window` if featured). #pim" "<C-next>"
   (lambda (arg)
     (interactive "p")
     (if (functionp 'ace-window)
         (let ((aw-ignore-current-orig aw-ignore-current))
           (setq aw-ignore-current t)
           (ace-window arg)
           (setq aw-ignore-current aw-ignore-current-orig)
           )
       (other-window 1 nil))))

  (map!
   :desc "Select another window in reverse cyclic ordering of windows or with `ace-window` if C-u prefix. #pim" "<C-prior>"
   (lambda (arg)
     "Select another window in reverse cyclic."
     (interactive "P")
     (if (and arg (functionp 'ace-window))
         (ace-window arg)
       (other-window -1 nil)))))


;; -------------------------------------------
;; * Filename completion anywhere with S-Tab *
(autoload 'comint-dynamic-complete-filename "comint" "" t)
;; List of suffixes to neglect during completion
(setq comint-completion-fignore (quote ("{" "}" "(" ")" "$" "=" "\"" "`" "'" "[" "]" "%" "<" ">")))
(map! :desc "Dynamically complete the filename under the cursor. #pim"
      "S-<iso-lefttab>" #'comint-dynamic-complete-filename
      "S-<tab>" #'comint-dynamic-complete-filename)

(map!
 :desc "Open recent file à la Chromium/Firefox. #pim"
 :g "C-S-t" #'consult-recent-file)

(cond
 ((modulep! :completion vertico)
  (map!
   :desc "Swith to buffer with the same workspace. #pim"
   "C-<tab>" #'+vertico/switch-workspace-buffer))
 ((modulep! :completion helm)
  (map!
   :desc "Swith to buffer with the same workspace. #pim"
   "C-<tab>" #'+helm/workspace-mini))
 ((modulep! :completion ivy)
  (map!
   :desc "Swith to buffer with the same workspace. Use 'C-x B' for extended buffer/file list. #pim"
   "C-<tab>" #'+ivy/switch-workspace-buffer)))

(when (modulep! :ui workspaces)
  ;; Add missing loading workspace in the Doom `C-c w` key prefix.
  (map! :leader
        :prefix ("w" . "workspaces/windows #pim")
        :desc "Load a workspace. #pim" "L" #'+workspace/load)

  (map!
   :prefix ("s-<tab>" "workspaces/windows #pim")
   :desc "Switch to last workspace. #pim" "<tab>" #'+workspace/other
   :desc "Switch to last workspace. #pim" "s-<tab>" #'+workspace/other
   :desc "Switch to choose workspace. #pim" "w" #'+workspace/switch-to))

;; (map! :leader
;;       :prefix ("TAB" . "switch #pim")
;;       :desc "Switch to last workspace. #pim" "TAB" #'+workspace/other
;;       :desc "Switch to workspace. #pim" "w" #'+workspace/switch-to))

;; ;; TODO : enable it
;; ;; ----------------------
;; ;; * Keys for info-mode *
;; (add-hook 'Info-mode
;;           (lambda ()
;;             (define-key Info-mode-map (kbd "<") 'Info-history-back)
;;             (define-key Info-mode-map (kbd ">") 'Info-history-forward)))

;; (map! :desc "Browse url at point. #pim" "C-c b" #'browse-url-at-point)
(map! :desc "Delete current window and buffer. #pim" "<f12>" 'pim/kill-window-and-buffer)
(map! :desc "Indent the whole buffer. #pim" "<C-S-iso-lefttab>" 'pim/indent-whole-buffer)

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

(map! :desc "Find file as root. #pim" "C-x C-r" #'doom/sudo-find-file)
(map! :desc "Jump/switch between the indentation column and the beginning of the line. #pim" "<home>" #'doom/backward-to-bol-or-indent)
(map! :desc "Jump/switch between the indentation column and the beginning of the line. #pim" "C-M-<prior>" #'doom/backward-to-bol-or-indent)
(map! :desc "Jump/switch between the last non-blank, non-comment character and the end of the line. #pim" "C-M-<next>" #'doom/forward-to-last-non-comment-or-eol)
(map! :desc "Jump/switch between the last non-blank, non-comment character and the end of the line. #pim" "<end>" #'doom/forward-to-last-non-comment-or-eol)
(map! :desc "Use fill line or region as auto-fill-mode does. #pim" "M-q" #'pim/fill)

(if pim-azertyp
    (progn
      (map! :desc "Comment/Uncomment the entire line and indent. #pim" "C-%"
            (lambda nil
              (interactive)
              (pim/?comment t)))
      (map! :desc "Comment/Uncomment the entire line but not indent. #pim" "C-ù" #'pim/?comment))
  (progn
    (map! :desc "Comment/Uncomment the entire line and indent (no indent if C-u prefix). #pim" "C-/"
          (lambda (arg)
            (interactive "P")
            (pim/?comment (not arg))))
    )
  )

;; Uncouple the keys <TAB> and <C-i> https://stackoverflow.com/q/1792326
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

;; ;; Fany insertion prefix.
(let ((keysm ";") ;; Semicolon and comma at the end of the line
      (keysm-desc "Fancy insert/delete semicolon at the end of the line. #pim")
      (keyco ",")
      (keyco-desc "Fancy insert/delete comma at the end of the line. #pim")
      (prefix "H-i")) ;; In fact, it is C-i
  (map! :prefix prefix
        :desc keysm-desc keysm #'pim/insert-semicol-at-end-of-line
        :desc keyco-desc keyco #'pim/insert-comma-at-end-of-line
        ;; Rebind flyspell default key-binding
        (:after flyspell
         :map flyspell-mode-map
         :desc keysm-desc keysm #'pim/insert-semicol-at-end-of-line
         :desc keyco-desc keyco #'pim/insert-comma-at-end-of-line
         )
        :desc "Insert a cool section comments. #pim" "s" #'pim/insert-comment-section
        :desc "Insert a cool section comments. #pim" "S" #'pim/insert-comment-sub-section
        )
  )


;; ;; TODO : Is it needed ?
;; ;; --------------------------------------------------------
;; ;; * Seeking a makefile recursively in directories higher *
;; (setq pim-compilation-filenames '("Makefile" "makefile"))

;; (defun pim-get-nearest-compilation-dir ()
;;   "Search for the compilation file traversing up the directory tree. Return the directory, not the file !
;; Src : http://www.emacswiki.org/cgi-bin/wiki/UsingMakefileFromParentDirectory"
;;   (let ((dir default-directory)
;;   (parent-dir (file-name-directory (directory-file-name default-directory)))
;;   (nearest-compilation-dir 'nil))
;;     (while (and (not (string= dir parent-dir))
;;     (not nearest-compilation-dir))
;;       (dolist (filename pim-compilation-filenames)
;;   (setq file-path (concat dir filename))
;;   (when (file-readable-p file-path)
;;     (setq nearest-compilation-dir dir)))
;;       (setq dir parent-dir
;;       parent-dir (file-name-directory (directory-file-name parent-dir))))
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


(map! :desc "Undo from undo-fu. #pim" "C-z" #'undo-fu-only-undo
      :desc "Redo from undo-fu. #pim" "C-S-z" #'undo-fu-only-redo)
(when pim-azertyp
  ;; C-/ is undo by default
  (map! :desc "Redo from undo-fu for azerty keyboard. #pim" "C-:" #'undo-fu-only-redo))

(advice-remove 'newline-and-indent #'+default--newline-indent-and-continue-comments-a)

;; (defun pim-newline-and-indent (&optional continue-comment)
;;   "Like `newline-and-indent' but handle Doom advice to handle `delete-selection-mode'."
;;   (interactive "P")
;;   (let ((+default-want-RET-continue-comments continue-comment))
;;     (when (and delete-selection-mode (region-active-p))
;;       (delete-active-region))
;;     (newline-and-indent)
;;     ))

(map!
 :desc "Like <RET> but enable continuing comment. #pim" "M-<RET>"
 (lambda (&optional arg)
   (interactive "*")
   (+default--newline-indent-and-continue-comments-a arg)
   ;; (pim-newline-and-indent t)
   )
 ;; :desc "Newline and indent but escape from continuing comment (use M-<ret> for continuing comment). #pim" "<RET>"
 ;; (lambda nil
 ;;   (interactive)
 ;;   (pim-newline-and-indent nil))
 )


(map!
 :desc "Toggle locally the modeline. #pim" "<M-f1>" #'hide-mode-line-mode
 :desc "Toggle globally the modeline. #pim" "<s-f1>" #'global-hide-mode-line-mode)

;; Non-breaking spaces with quotes please.
(when pim-azertyp
  (map!
   :desc "Insert proper French quotation with non breaking spaces. #pim" "«"
   (lambda nil
     (interactive)
     (insert
      "« ")(insert
      " »")(backward-char
      2))
   :desc "Add non breaking spaces before the closing French quote. #pim" "»"
   (lambda nil (interactive) (insert " »"))))


;; ;; TODO : to be enabled
;; ;; --------------------------------
;; ;; * Highlight the current column *
;; (when (locate-library "column-marker")
;;   (autoload 'column-marker-1 "column-marker" "Highlight a column." t)
;;   ;; http://www.emacswiki.org/cgi-bin/wiki/col-highlight.el
;;   (require 'col-highlight)
;;   ;; Raccourci sur [f10]
;;   (global-set-key (kbd "<f10>") 'column-highlight-mode))

;;;###package jumpc
(use-package!
    jumpc
  ;; :defer t ;; defered does not work…
  ;; :commands (jumpc-jump-backward jumpc-jump-forward)
  :preface
  (if pim-azertyp
      (progn
        (map! :desc "Jump to prev cursor position. #pim" "C-<" #'jumpc-jump-backward)
        (map! :desc "Jump to next cursor position. #pim" "C->" #'jumpc-jump-forward))
    (progn
      (map! :desc "Jump to prev cursor position. #pim" "<f8>" #'jumpc-jump-backward)
      ((map! :desc "Jump to next cursor position. #pim" "<f9>" 'jumpc-jump-forward)))
    )
  :config
  (jumpc))

;; (map!
;;  :after better-jumper
;;  :desc "Jump to prev cursor position. #pim" "C-<" #'better-jumper-jump-backward)

;; (map!
;;  :after better-jumper
;;  :desc "Jump to next cursor position. #pim" "C->" #'better-jumper-jump-forward)

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
(map! :desc "Start the definition of a macro. #pim" "S-<f4>" #'kmacro-start-macro
      :desc "Ending the definition of a macro. #pim" "<f4>" #'kmacro-end-or-call-macro
      :desc "Edit the last defined macro. #pim" "<C-f4>" #'kmacro-edit-macro)

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
      :prefix ("8" . "utf-8 #pim")
      :desc "Choose and insert an emoji glyph #pim" "i" #'emoji-insert)

;; (map! :leader
;;       :prefix ("8" . "utf-8 #pim")
;;       :desc "Choose and insert an emoji glyph #pim" "e" #'emoji-insert
;;       (:prefix ("1" . "fraction one #pim")
;;        :desc "Fraction one half #pim" "2" "½"
;;        :desc "Fraction one third #pim" "3" "⅓"
;;        :desc "Fraction one quarter #pim" "4" "¼"
;;        :desc "Fraction one fifth #pim" "5" "⅕"
;;        :desc "Fraction one sixth #pim" "6" "⅙"
;;        :desc "Fraction one seventh #pim" "7" "⅐"
;;        :desc "Fraction one eighth #pim" "8" "⅛"
;;        :desc "Fraction one ninth #pim" "9" "⅑"
;;        :desc "Fraction one tenth #pim" "0" "⅒"
;;        )

;;       (:prefix ("f" . "face #pim")
;;        :desc "🙂 #pim" "s" "🙂"
;;        :desc "😀 #pim" "g" "😀"
;;        :desc "😬 #pim" "G" "😬"
;;        :desc "😒 #pim" "u" "😒"
;;        :desc "😞 #pim" "d" "😞"
;;        :desc "😖 #pim" "c" "😖"
;;        :desc "😉 #pim" "w" "😉"
;;        :desc "😂 #pim" "j" "😂"
;;        :desc "😟 #pim" "W" "😟"
;;        :desc "😅 #pim" "C" "😅"
;;        :desc "😱 #pim" "S" "😱"
;;        :desc "😆 #pim" "e" "😆"
;;        :desc "😭 #pim" "l" "😭"
;;        )

;;       (:prefix ("s" . "symbol #pim")
;;        :desc "⚠️ #pim" "w" "⚠️"
;;        :desc "⚡ #pim" "z" "⚡"
;;        :desc "• #pim" "b" "•"
;;        )

;;       (:prefix ("b" . "bullet #pim")
;;        :desc "• #pim" "b" "•"
;;        :desc "‣ #pim" "t" "‣"
;;        :desc "⁃ #pim" "h" "⁃"
;;        :desc "◘ #pim" "i" "◘"
;;        :desc "◦ #pim" "w" "◦"
;;        :desc "☑ #pim" "c" "☑"
;;        :desc "☒ #pim" "x" "☒"
;;        :desc "❧ #pim" "f" "❧"
;;        :desc "☙ #pim" "F" "☙"
;;        :desc "⦿ #pim" "C" "⦿"
;;        )

;;       (:prefix ("a" . "arrow #pim")
;;        :desc "🠕 #pim" "u" "🠕"
;;        :desc "🠖 #pim" "r" "🠖"
;;        :desc "🠔 #pim" "l" "🠔"
;;        :desc "🠗 #pim" "d" "🠗"

;;        :desc "🠙 #pim" "U" "🠙"
;;        :desc "🠚 #pim" "R" "🠚"
;;        :desc "🠘 #pim" "L" "🠘"
;;        :desc "🠛 #pim" "D" "🠛"
;;        )
;;       )

(map! :desc "Switch to the next user buffer. #pim" "<mouse-9>" #'pim/next-user-buffer)
(map! :desc "Switch to the previous user buffer. #pim" "<mouse-8>" #'pim/previous-user-buffer)
(map!
 :map minibuffer-local-map
 :desc "Switch to the previous user buffer. #pim" "C-<tab>" (lambda nil
                                                              (interactive)
                                                              (execute-kbd-macro (read-kbd-macro "<RET>")))
 :desc "Switch to the next previous user buffer. #pim" "C-S-<iso-lefttab>" (lambda nil
                                                                             (interactive)
                                                                             (execute-kbd-macro (read-kbd-macro "<down> <RET>")))
 )


;; TODO : better key bindings than lsp-describe-thing-at-point ?
;; https://github.com/emacs-lsp/lsp-ui/

(provide 'pimacs/keys/+emacs)
;; +emacs.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/keys/+emacs.el")
;; End:
