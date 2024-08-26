;;; pimacs/dired/autoload.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun pim/dired-rename ()
  "Use multiple renaming if multiple files are marked.
Otherwise, use a buffer in which all the names of files are editable."
  (interactive)
  (require 'wdired)
  (if (nth 1 (save-excursion
               (dired-map-over-marks
                (dired-get-filename nil t)
                nil nil t)))
      (dired-do-rename) ;; au moins un fichier marqué.
    (wdired-change-to-wdired-mode)))

(provide 'pimacs/dired)
;;; autoloaded.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/dired/autoload.el")
;; End:
