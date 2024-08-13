;;; pimacs/pim-default/package.el -*- no-byte-compile: t; -*-
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

;; Code:


;; ----------------------------------------
;; * HTML rendering for buffers and files *
;; Just call `htmlize-view-buffer' to show the current buffer in
;; your web browser.
;;;###package html-view
(use-package!
 htmlize-view
 :defer t
 ;; :commands (htmlize-buffer htmlize-file htmlize-many-files
 ;;                           htmlize-many-files-dired
 ;;                           htmlize-region htmlize-view-buffer)
 :config
(setq
 htmlize-convert-nonascii-to-entities nil
 htmlize-html-charset "utf-8")
 (htmlize-view-add-to-files-menu))

;; -----------------
;; * Box some text *
;; Command M-x boxquote-...
(use-package! boxquote
              :defer t)

(use-package! which-func
              :commands (which-function-mode)
              :init (which-function-mode 1)
              :defer t)

;; Local variables:
;; coding: utf-8
;; End:
