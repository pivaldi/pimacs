;;; Package --- pi c/c++ programming langage configuration.
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

;;;; Commentary:

;;; Code:

;; Standardize indentation
(defun pi/c-indent-setup ()
  (setq-default c-basic-offset 2)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'brace-list-open '0)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'statement-case-open '0)
  (c-set-offset 'arglist-cont-nonempty '4)
  (c-set-offset 'arglist-intro 'c-basic-offset))

;; (add-hook 'c-mode-hook 'pi-c-indent-setup)
(add-hook 'c++-mode-hook 'pi/c-indent-setup)

(provide 'pi-c)
;;; pi-c.el ends here

;; Local variables:
;; coding: utf-8
;; End:
