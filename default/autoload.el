;;; Package pymacs/defaul --- See README.md -*- lexical-binding: t; -*-
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

;;; Code:

;;;###autodef
(defvar pim/scissor-pattern "✂·······"
  "String pattern to insert in `pim/scissor`.")

;;;###autoload
(defun pim/scissor ()
  "Insert a line of 'pim/scissor-pattern' in the buffer."
  (interactive)
  (or (bolp) (beginning-of-line 2))
  (while (<= (current-column) (- (or fill-column 70) (length pim/scissor-pattern)))
    (insert pim/scissor-pattern))
  (newline))

;;;###autoload
(defun pim-make-script-executable ()
  "If file start with a shebang, make buffer file name executable.
See http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave"
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
(defun pim-get-filename-uc-part (&optional filename offset trim-file-name)
  "Get the longest terminating first upper case part of a file name.
- If FILENAME is missing, use the filename attached to the current buffer if
  any.
- If OFFSET an integer is not nil, truncates the first characters by OFFSET
  chars.
- If TRIM-FILE-NAME an bool is not nil, remove the filename part.

Example :
If FILENAME is /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php the function
will return :
- /App/CPro/Model/Poi/Zone without option
- App/CPro/Model/Poi/Zone with option OFFSET 1
- /App/CPro/Model/Poi/ with option TRIM-FILE-NAME t"
  (let* ((offset (or offset 0))
         (filename (file-name-sans-extension (or filename (buffer-file-name))))
         (filename (if trim-file-name
                       (file-name-directory filename) filename)))
    (substring filename
               (+ offset
                  (let ((case-fold-search nil))
                    (or (string-match
                         "\\\(/[A-Z][a-zA-Z0-9.-_]+\\\)+$" filename)
                        (- (length filename) offset)))))))

(provide 'pimacs/default/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/autoload.el")
;; End:
