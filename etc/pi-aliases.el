;;; Package --- pi aliases
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

;; Short aliases starts by underscore.

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:


(defalias '_ll 'longlines-mode)
(defalias '_rb 'revert-buffer)
(defalias '_ar 'align-regexp)
(defalias '_gf 'grep-find)
(defalias '_ff 'find-name-dired)
(defalias '_ib 'ibuffer-list-buffers)
(defalias '_sir 'string-insert-rectangle)

;; Replace yes/no <RET> by y/n.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'pi-aliases)
;;; pi-aliases.el ends here

;; Local variables:
;; coding: utf-8
;; End:
