;;; pimacs/default/+advises.el -*- lexical-binding: t; -*-
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

;;; Description

;; Possibility to filter unwanted displayed Emacs warning by message regexps.
;; The variable `pim-warning-suppress-message-regexps` can be customize to add filter.

;;; Commentary:

;; Feature done by adding an advise on the function `display-warning`.

;;; Code:

(defcustom pim-warning-suppress-message-regexps nil
  "List of warning messages not to display immediately.
See also `warning-suppress-log-types'."
  :type '(repeat (repeat string))
  :group 'pimacs)

(defun pim-display-warning-advise (type message &optional level buffer-name)
  "Allow filtering Emacs warning messages by regexps.
See the varibale `pim-warning-suppress-message-regexps'."
  (catch 'exit
    (dolist (regexp pim-warning-suppress-message-regexps)
      (when (string-match-p regexp message) (throw 'exit nil))
      )
    (throw 'exit t)
    ))

(add-function :before-while (symbol-function 'display-warning) #'pim-display-warning-advise)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/+advise.el")
;; End:
