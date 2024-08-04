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

;; Commentary:

;; THANKS:

;; BUGS:

;; INSTALLATION:

;; Code:

;;;###autoload
(defun pim-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable.
From http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

;;;###autoload
(defun pim/kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Passes ARG to command `kill-line' when provided.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation 1)
    (kill-line arg)))

;;;###autoload
(defun pim/buffer-file-name (prefix &optional killit)
  "Show the buffer file name (if any)
and make it the latest kill in the kill ring if KILLIT is t.
With PREFIX, write in the current buffer."
  (interactive "P")
  (if buffer-file-name
      (if prefix
          (insert buffer-file-name)
        (if killit
            (let ((select-enable-primary t))
              (kill-new (message buffer-file-name))
              (gui-select-text (message buffer-file-name)))
          (message buffer-file-name)))
    (message "No file-name attached to the bufer")))

;;;###autoload
(defun pim/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))


;;;###autoload
(defun pim/delete-sexp (&optional arg)
  "Delete the sexp (balanced expression) following point.
With ARG, delete that many sexps after point.
Negative arg -N means delete N sexps before point.
This command assumes point is not in a string or comment."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (delete-region opoint (point))))

;;;###autoload
(defun pim/backward-delete-sexp (&optional arg)
  "Delete the sexp (balanced expression) preceding point.
With ARG, delete that many sexps before point.
Negative arg -N means delete N sexps after point.
This command assumes point is not in a string or comment."
  (interactive "p")
  (delete-sexp (- (or arg 1))))

;;;###autoload
(defun pim/kill-window-and-buffer()
  "* Delete current window and buffer."
  (interactive)
  (let ((wind (selected-window)))
    (if (not (buffer-file-name))
        (progn
          (let ((buffer-modified-p nil))
          (set-window-dedicated-p wind nil)
          (kill-buffer-and-window)))
      (progn
        (kill-current-buffer)
        (condition-case nil (delete-window) (error nil))))))

;;;###autoload
(defun pim/indent-whole-html-buffer nil
  "Indent the whole buffer except <pre> part in html mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((ppoint (point)))
      (while (search-forward-regexp "<pre.*?>"  (point-max) t)
        (indent-region ppoint (point) nil)
        (search-forward-regexp "</pre>" (point-max) t)
        (setq ppoint (+ 1 (point))))
      (indent-region ppoint (point-max) nil))))

;;;###autoload
(defun pim/indent-whole-buffer nil
  "Indent the whole buffer.
If the mark `(concat comment-start \"--indent after--\")`
is found in the buffer the indentation start after the last mark found."
  (interactive)
  (save-excursion
    (if (assoc-string major-mode (list "xhtml-mode" "html-mode" "nxhtml-mode"))
        (pim/indent-whole-html-buffer)
      (progn
        (goto-char (point-min))
        (let ((ppoint (point)))
          (while (search-forward-regexp
                  (concat
                   (regexp-quote comment-start)
                   "*--noindent--") (point-max) t)
            (forward-line -1)
            (indent-region ppoint (point) nil)
            (forward-line 2)
            (setq ppoint (point)))
          (indent-region ppoint (point-max) nil))))))

;;;###autoload
(defun pim/find-file-root ()
  "Find file as root."
  (interactive)
  (let ((file (read-file-name "Find file as root : ")))
    (find-file (concat "/su::" file))))

;;;###autoload
(defun pim/home()
  "Move cursor at beginning of line or first non blank character.
Depending where the cursor is."
  (interactive)
  (let ((pt_indent (point)))
    (back-to-indentation)
    (if (eq pt_indent (point))
        (beginning-of-line))
    ))

;; See also comment-dwim
;;;###autoload
(defun pim/?comment (&optional indentp)
  "Comment/Uncomment the entire line and indent if arg INDENTP is t."
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

;;;###autoload
(defun pim/insert-comment-section ()
  "Insert a section comments."
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
    (forward-line)
    (beginning-of-line)))

;;;###autoload
(defun pim/insert-comment-sub-section ()
  "Insert a section sub comments."
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
    (forward-line)
    (beginning-of-line)))

;;;###autoload
(defun pim/fill ()
  "Use fill line or region as auto-fill-mode does.";
  (interactive)
  (save-excursion
    (if mark-active
        (fill-region-as-paragraph (point) (mark))
      (do-auto-fill))))

(defun pi-insert-str-at-end-of-line (str &optional removeStrList)
  (save-excursion
    (end-of-line)
    (while
        (progn
          (when (member (char-to-string (char-before)) removeStrList)
            (delete-backward-char 1) t
            )
          )
        )
    (if (not (char-equal (char-before) (string-to-char str))) (insert str)
      (delete-backward-char 1))))

;;;###autoload
(defun pim/insert-semicol-at-end-of-line  nil
  (interactive)
  (pi-insert-str-at-end-of-line ";" '("," ":" "." "#" "%" "/")))

;;;###autoload
(defun pim/insert-comma-at-end-of-line nil
  (interactive)
  (pi-insert-str-at-end-of-line "," '(";" ":" "." "#" "%" "/")))



;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/autoload.el")
;; End: