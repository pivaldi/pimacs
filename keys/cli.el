#!/usr/bin/env doomscript
;;; pimacs/keys/cli.el --- See README.md -*- lexical-binding: t; -*-
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

:;; Commentary:

;;; Code:

(load! "../cli.el")

(defcli! pim-to-refcard ()
         "Export the PIMacs global-map key bindings into the file key-bindings-refcard.md of the project."
         (let ((byte-compile-current-file t))
           (require 'doom-start)
           (pim-keys-bindings-to-md-refcard "global-map" 'keys)
           )
         )

(provide 'pimacs/keys/cli)
;;; cli.el ends here

;; Local variables:
;; coding: utf-8
;; End:
