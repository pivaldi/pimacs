;; -*- no-byte-compile: t; -*-
;;; pimacs/flyspell/packages.el
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

;; `flyspell-correct' provides `flyspell-correct-at-point', used by
;; `pim-flyspell-correct' (bound to M-$).  Declaring it here makes this module
;; self-sufficient instead of depending on Doom's `:checkers spell +flyspell'
;; module to install it.  Declaring the same package in both modules is fine:
;; Doom deduplicates package declarations.

;;; Code:

(package! flyspell-correct)

(provide 'pimacs/flyspell/packages)
;;; packages.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/flyspell/packages.el")
;; End:
