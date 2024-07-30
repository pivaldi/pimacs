;;; Package --- Custom definition of variables used by pimacs
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

(defcustom pi-auto-fill-mode-hook-alist
  '(text-mode-hook org-mode-hook)
  "List of hooks for which we want auto-fill-mode --automatic break of long lines--."
  :type 'hook
  :group 'pimacs-coding)

(provide 'pi-defcustom)
;;; pi-defcustom.el ends here

;; Local variables:
;; coding: utf-8
;; End:
