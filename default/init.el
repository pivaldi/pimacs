;;; -*- lexical-binding: t; -*-
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

;; Set the debug option to enable a backtrace when
;; problems occur.
(defvar pim-error-msgs (list)
  "List of errors encountered when loading pim-configuration files.")

(defvar pim-keymapname-alist (list)
  "List (\"module name\" . '(keymapname1 keymapname2 etc)  configured by PIMacs.")

(defcustom pim-auto-fill-mode-hooks
  '(text-mode-hook org-mode-hook)
  "List of hooks for which we want auto-fill-mode --automatic break of long lines--."
  :type 'hook
  :group 'pimacs-coding)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/init.el")
;; End:
