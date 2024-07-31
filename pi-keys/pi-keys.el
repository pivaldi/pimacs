;;; Package -- pimacs keys configuration
;;; Copyright (c) 2016, Philippe Ivaldi <www.piprime.fr>
;; Version: $Id: pi-package.el,v 0.0 2016/03/23 15:51:19 Exp $

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

;;; Code:

(register-definition-prefixes "pi-functions" '("pi/"))

(map! :desc "If at end of line, join with following; otherwise kill line." "C-k" 'pi/kill-and-join-forward)

;; ;; TODO : Use the package easy-kill
(map! :desc "filename in the minibuffer, in the buffer with C-u" "<f8>" 'pi/buffer-file-name)
(map! :desc "echo filename in the minibuffer and put in the kill ring" "<S-f8>"
      (lambda nil
        (interactive)
        (pi/buffer-file-name nil t)))

(map! :desc "Delete characters backward until encountering the beginning of a word."
      "<C-backspace>" 'pi/backward-delete-word)

(map! "<M-backspace>" 'pi/backward-delete-sexp)
(map! "<M-delete>"
      (lambda ()
        (interactive)
        (beginning-of-line)
        (kill-line)))

;; ------------------------
;; * Key for other-window *
(map!
 :desc "Select another window in cyclic ordering of windows or with `ace-window`." "<C-next>"
 (lambda (arg)
   (interactive "p")
   (if (functionp 'ace-window)
       (ace-window arg)
       (other-window 1 nil))))
(map!
 :desc "Select another window in backwards ordering of windows or with `ace-window`." "<C-prior>"
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
(map! :desc "Dynamically complete the filename under the cursor"
      "<S-iso-lefttab>" #'comint-dynamic-complete-filename
      "<S-tab>" #'comint-dynamic-complete-filename)

(map! (:after consult)
      :desc "Open recent file."
      :g "C-S-t" 'consult-recent-file)

(map! (:after vertico)
      :desc "Swith to buffer with vertico."
      "<C-tab>" '+vertico/switch-workspace-buffer)

(map! (:after vertico)
      :desc "Swith to buffer with vertico in the same workspace."
      "<s-tab>" (lambda ()
                  (interactive)
                  (+vertico/switch-workspace-buffer t)))

(map! (:after ido)
      :desc "Swith to buffer with ido."
      "<C-tab>" 'ido-switch-buffer)

;; ;; TODO : enable it
;; ;; ----------------------
;; ;; * Keys for info-mode *
;; (add-hook 'Info-mode
;;           (lambda ()
;;             (define-key Info-mode-map (kbd "<") 'Info-history-back)
;;             (define-key Info-mode-map (kbd ">") 'Info-history-forward)))

(map! :desc "Browse url at point." "C-c b" #'browse-url-at-point)

(map! :desc "Delete current window and buffer." "<f12>" 'pi/kill-window-and-buffer)

(map! :desc "Delete current window and buffer." "<C-S-iso-lefttab>" 'pi/indent-whole-buffer)

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

(map! :desc "Find file as root" "C-x C-r" 'pi/find-file-root)

(map! :desc "* Move cursor at beginning of line or first non blank character." "<home>" 'pi/home)

;; ;; TODO : to be tested
;; (defun pi/fill ()
;;   "Use fill line or region as auto-fill-mode does"
;;   (interactive)
;;   (save-excursion
;;     (if mark-active
;;         (fill-region-as-paragraph (point) (mark))
;;       (do-auto-fill))))
;; (map! :desc "Use fill line or region as auto-fill-mode does." "M-q" 'pi/fill)


(map! :desc "Comment/Uncomment the entire line and indent" "C-%" (lambda nil
                                                                   (interactive)
                                                                   (pi/?comment t)))
(map! :desc "Comment/Uncomment the entire line but not indent" "C-ù" 'pi/?comment)

;; ;; TODO : to be implemented
;; ;; Semicolon and comma at the end of the line
;; (let ((keysm (kbd "C-;"))
;;       (keyco (kbd "C-,")))
;;   (global-set-key keysm 'pi-insert-semicol-at-end-of-line)
;;   (if (boundp 'flyspell-mode-map)
;;       (define-key flyspell-mode-map
;;         keysm 'pi-insert-semicol-at-end-of-line))
;;   (global-set-key keyco 'pi-insert-comma-at-end-of-line)
;;   (if (boundp 'flyspell-mode-map)
;;       (define-key flyspell-mode-map
;;         keyco 'pi-insert-comma-at-end-of-line)))

(map! :desc "Insert a section comments." "C-Μ" 'pi/insert-comment-section)
(map! :desc "Insert a section comments." "C-*" 'pi/insert-comment-sub-section)


;; ;; TODO : Is it needed ?
;; ;; --------------------------------------------------------
;; ;; * Seeking a makefile recursively in directories higher *
;; (setq pi-compilation-filenames '("Makefile" "makefile"))

;; (defun pi-get-nearest-compilation-dir ()
;;   "Search for the compilation file traversing up the directory tree. Return the directory, not the file !
;; Src : http://www.emacswiki.org/cgi-bin/wiki/UsingMakefileFromParentDirectory"
;;   (let ((dir default-directory)
;; 	(parent-dir (file-name-directory (directory-file-name default-directory)))
;; 	(nearest-compilation-dir 'nil))
;;     (while (and (not (string= dir parent-dir))
;; 		(not nearest-compilation-dir))
;;       (dolist (filename pi-compilation-filenames)
;; 	(setq file-path (concat dir filename))
;; 	(when (file-readable-p file-path)
;; 	  (setq nearest-compilation-dir dir)))
;;       (setq dir parent-dir
;; 	    parent-dir (file-name-directory (directory-file-name parent-dir))))
;;     nearest-compilation-dir))

;; (defun pi-compile-above-makefile ()
;;   (interactive)
;;   (let* ((mkf (pi-get-nearest-compilation-dir))
;;          (default-directory (directory-file-name mkf)))
;;     (if default-directory
;;         (progn
;;           (cd default-directory)
;;           (compile "[ -e ./ovyaproject.rc ] && source ovyaproject.rc; make")))))
;; (global-set-key (kbd "<f9>") 'pi-compile-above-makefile)


;; ;; TODO : to be implemented
;; ;; -----------------------------------------------
;; ;; * Scroll down the page rather than the cursor *
;; ;; use the key "Scroll Lock Num" ("Num Défil" in french) to toggle.
;; ;; C-up et C-down
;; (autoload 'pi-scroll-lock-mode "pi-scroll-lock" "Toggle pi-scroll-lock-mode." t)
;; (global-set-key (kbd "<Scroll_Lock>") 'pi-scroll-lock-mode)
;; ;; Switches to hl-line-mode when the cursor is locked.
;; (setq pi-scroll-hl t)


;; C-/ is undo by default
(map! :desc "Redo !" "C-:" 'redo)

;; Non-breaking spaces with quotes please.
(map! :desc "Non-breaking spaces with quotes please." "«"
      (lambda nil
        (interactive)
        (insert
         "« ")(insert
         " »")(backward-char
         2)))
(map! :desc "Non-breaking spaces with quotes please." "»" (lambda nil (interactive) (insert " »")))


;; ;; TODO : to be enabled
;; ;; --------------------------------
;; ;; * Highlight the current column *
;; (when (locate-library "column-marker")
;;   (autoload 'column-marker-1 "column-marker" "Highlight a column." t)
;;   ;; http://www.emacswiki.org/cgi-bin/wiki/col-highlight.el
;;   (require 'col-highlight)
;;   ;; Raccourci sur [f10]
;;   (global-set-key (kbd "<f10>") 'column-highlight-mode))

;; ;; -----------------------------------
;; ;; * Return to the previous position *
;; (require 'jumptoprevpos)
;; (global-set-key (kbd "C-<") 'jump-to-prev-pos)
;; (global-set-key (kbd "C->") 'jump-to-next-pos)

;; ;; Define C-x up | C-x down | C-x right | C-x left to resize the windows
;; (require 'pi-resize-window "pi-resize-window.el" t)

;; ;; Default is dabbrev-expand mais hippie-expand est plus généraliste !
;; (defcustom pi-use-hippie-expand-p nil
;;   "When set to true use hippie-expand instead of dabbrev-expand.
;; Default key binding is M-/
;; "
;;   :type 'boolean
;;   :group 'pimacs)

;; (if pi-use-hippie-expand-p
;;     (global-set-key "\M-/" 'hippie-expand)
;;   (global-set-key "\M-/" 'dabbrev-expand))

;; ;; ----------------------
;; ;; * disable insert key *
;; (global-set-key (kbd "<insert>")
;;                 (lambda nil
;;                   (interactive)
;;                   (message "Insert is desabled. Use \"M-x overwrite-mode\" instead")))

;; ;; ----------------------------
;; ;; * C-f1 toggle the menu bar *
;; (global-set-key (kbd "<C-f1>") 'menu-bar-mode)

;; ;; ------------------------
;; ;; * Pour créer une macro *
;; ;; Début de définition d'une macro
;; (if (fboundp 'kmacro-start-macro)
;;     (global-set-key (kbd "S-<f4>") 'kmacro-start-macro)
;;   ;; Termine la définition en cours sinon execute la dernière.
;;   (global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)
;;   ;; Edite la dernière macro
;;   (global-set-key (kbd "<C-f4>") 'kmacro-edit-macro))


;; ;; ------------------------
;; ;; * Expand M-g goto-xxx *
;; (global-set-key (kbd "M-g d") 'beginning-of-defun)


;; Local variables:
;; coding: utf-8
;; End:
