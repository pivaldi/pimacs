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

(defun pi/kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Passes ARG to command `kill-line' when provided.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation 1)
    (kill-line arg)))

(map! "C-k" 'pi/kill-and-join-forward)

;; ---------------------------------------------
;; * Obtenir le nom complet du fichier courant *
(defun pi/buffer-file-name (prefix &optional killit)
  "Show the `buffer-file-name' (if any) and make it
the latest kill in the kill ring if `killit' is t.
With prefix, write in the current buffer."
  (interactive "P")
  (if buffer-file-name
      (if prefix
          (insert buffer-file-name)
        (if killit
            (let ((x-select-enable-primary t))
              (kill-new (message buffer-file-name))
              (x-select-text (message buffer-file-name)))
          (message buffer-file-name)))
    (message "No file-name attached to the bufer")))
;; F8     : echo filename in the minibuffer
;; C-u F8 : insert filename in the current buffer
(map! :desc "echo filename in the minibuffer" "<f8>" 'pi/buffer-file-name)
;; S-f8   : echo filename in the minibuffer and put in the kill ring.
(map! :desc "echo filename in the minibuffer and put in the kill ring" "<S-f8>"
      (lambda nil
        "Show the `buffer-file-name' (if any) and make it
the latest kill in the kill ring."
        (interactive)
        (pi/buffer-file-name nil t)))

;; ------------------------------
;; * Suppression rapide de mots *
(defun pi/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(map! :desc "Delete characters backward until encountering the beginning of a word."
      "<C-backspace>" 'pi/backward-delete-word)

(defun delete-sexp (&optional arg)
  "Delete the sexp (balanced expression) following point.
With ARG, delete that many sexps after point.
Negative arg -N means delete N sexps before point.
This command assumes point is not in a string or comment."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (delete-region opoint (point))))

(defun backward-delete-sexp (&optional arg)
  "Delete the sexp (balanced expression) preceding point.
With ARG, delete that many sexps before point.
Negative arg -N means delete N sexps after point.
This command assumes point is not in a string or comment."
  (interactive "p")
  (delete-sexp (- (or arg 1))))

(map! "<M-backspace>" 'backward-delete-sexp)
(map! "<M-delete>"
      (lambda ()
        (interactive)
        (beginning-of-line)
        (kill-line)))

;; ------------------------
;; * Key for other-window *
(map! :desc "Select another window in cyclic ordering of windows." "<C-next>"
      (lambda ()
        (interactive)
        (other-window 1 nil)))
(map! :desc "Select another window in backwards ordering of windows." "<C-prior>"
      (lambda ()
        (interactive)
        (other-window -1 nil)))

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

(defun pi/kill-window-and-buffer()
  "* Delete current window and buffer."
  (interactive)
  (kill-current-buffer)
  (condition-case nil (delete-window) (error nil)))

(map! :desc "Delete current window and buffer." "<f12>" 'pi/kill-window-and-buffer)

(defun pi/indent-whole-html-buffer nil
  "Indent the whole buffer except <pre> part in html mode."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((ppoint (point)))
      (while (search-forward-regexp "<pre.*?>"  (point-max) t)
        (indent-region ppoint (point) nil)
        (search-forward-regexp "</pre>" (point-max) t)
        (setq ppoint (+ 1 (point))))
      (indent-region ppoint (point-max) nil))))

;; ---------------------------
;; * Indent the whole buffer *
(defun pi/indent-whole-buffer nil
  "Indent the whole buffer. If the mark `(concat comment-start \"--indent after--\")`
is found in the buffer the indentation start after the last mark found."
  (interactive)
  (save-excursion
    (if (assoc-string major-mode (list "xhtml-mode" "html-mode" "nxhtml-mode"))
        (pi/indent-whole-html-buffer)
      (progn
        (beginning-of-buffer)
        (let ((ppoint (point)))
          (while (search-forward-regexp
                  (concat
                   (regexp-quote comment-start)
                   "*--noindent--") (point-max) t)
            (previous-line)
            (indent-region ppoint (point) nil)
            (next-line 2)
            (setq ppoint (point)))
          (indent-region ppoint (point-max) nil))))))

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

(defun pi/find-file-root ()
  "* Find file as root."
  (interactive)
  (let ((file (ido-read-file-name "Find file AS ROOT : ")))
    (find-file (concat "/su::" file))))
(map! :desc "Find file as root" "C-x C-r" 'pi/find-file-root)

(defun pi/home()
  "* Move cursor at beginning of line or first non blank character
depending where the cursor is."
  (interactive)
  (let ((pt_indent (point)))
    (back-to-indentation)
    (if (eq pt_indent (point))
        (beginning-of-line))
    ))

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

;; See also comment-dwim
(defun pi-?comment (&optional indentp)
  "* Comment/Uncomment the entire line and indent if arg INDENTP is t."
  (interactive)
  (save-excursion
    (if mark-active
        (let ((br (if (< (point) (mark)) (point) (mark)))
              (be (if (> (point) (mark)) (point) (mark))))
          (comment-or-uncomment-region br be)
          (and indentp (indent-region br be)))
      (let ((br (progn  (back-to-indentation) (point)))
            (be (progn (end-of-line) (point))))
        (comment-or-uncomment-region br be)
        (and indentp (indent-according-to-mode))))))

(map! :desc "Comment/Uncomment the entire line and indent" "C-%" (lambda nil
                                                                   (interactive)
                                                                   (pi-?comment t)))
(map! :desc "Comment/Uncomment the entire line but not indent" "C-%" 'pi-?comment)

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

(defun pi/insert-comment-section ()
  "* To insert a section comments."
  (interactive)
  (let* ((str (if (and mark-active transient-mark-mode)
                  (prog1
                      (buffer-substring (region-beginning) (region-end))
                    (delete-region (region-beginning) (region-end)))
                (read-string "Section comment: ")))
         (str_ (if (string= str "") " - " str))
         (v1 (make-string (- fill-column 15) ?=))
         (v2 (- fill-column 15 (length str_)))
         (spce (make-string (floor v2 2) ?.))
         (pt (progn (beginning-of-line) (point))))
    (insert (concat "*" v1 "*\n*" spce str_ spce
                    (unless (= (ceiling v2 2) (/ v2 2)) ".")
                    "*\n*" v1 "*"))
    (comment-region pt (point))
    (next-line)
    (beginning-of-line)))
(map! :desc "Insert a section comments." "C-Μ" 'pi/insert-comment-section)

(defun pi/insert-comment-sub-section ()
  "* To insert a section sub comments"
  (interactive)
  (let* ((str (if (and mark-active transient-mark-mode)
                  (prog1
                      (buffer-substring (region-beginning) (region-end))
                    (delete-region (region-beginning) (region-end)))
                (read-string "Sub section comment: ")))
         (str_ (if (string= str "") " - " str))
         (v1 (make-string (+ (length str_) 4) ?-))
         (pt (progn (beginning-of-line) (point))))
    (insert (concat  v1 "\n* " str_ " *"))
    (comment-region pt (point))
    (next-line)
    (beginning-of-line)))
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
