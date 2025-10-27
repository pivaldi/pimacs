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

;; -----------------
;; * Box some text *
;; Command M-x boxquote-...
;;;###package boxquote
(package! boxquote)

(if (file-directory-p "/home/pi/code/pi/emacs/unaccent.el/")
    (package! unaccent.el
      :recipe (:local-repo "/home/pi/code/pi/emacs/unaccent.el/"
               :branch "master"
               :files ("*.el")))
  (package! unaccent.el
    :recipe (:host github :repo "pivaldi/unaccent.el")))

;; Local variables:
;; coding: utf-8
;; End:
