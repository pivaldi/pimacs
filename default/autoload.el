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

(provide 'pimacs/default/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; End:
