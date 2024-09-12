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

(defalias '_ll 'visual-line-mode "Long Line Mode.")
(defalias '_rb 'revert-buffer "Revert the Buffer.")
(defalias '_ar 'align-regexp "Align Regexp.")
(defalias '_gf 'grep-find "Fancy Grep Find.")
(defalias '_rg 'consult-ripgrep "Search with `rg' for files with input.")
(defalias '_ff 'find-name-dired "Search DIR recursively for files matching the globbing.")
(defalias '_ib 'ibuffer-list-buffers "Display a list of buffers, in another window.")
(defalias '_sir 'string-insert-rectangle "Insert STRING on each line of region-rectangle, shifting text right.")

;; Replace yes/no <RET> by y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Local variables:
;; coding: utf-8
;; End:
