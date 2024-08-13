;;; pimacs/default/mode.el -*- lexical-binding: t; -*-
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

;; Move cursor by camelCase
(global-subword-mode 1)

;; After selecting a region, inserting a new character will overwrite
;; the whole region
(delete-selection-mode 1)

;; Show line/column number in the mode line.
(line-number-mode t)
(column-number-mode t)
;; Read automatically  .gz and .zip files.
(auto-compression-mode 1)

;; Read images by default.
(auto-image-file-mode t)

;; Enable whitespace visualization globally.
(global-whitespace-mode)

;; Enables coloring in all modes.
(when (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

(when (fboundp 'global-display-fill-column-indicator-mode)
  (global-display-fill-column-indicator-mode 1))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/+modes.el")
;; End:
