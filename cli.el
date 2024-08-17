;;; Package pimacs/cli --- See README.md -*- lexical-binding: t; -*-
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

;; This file is loaded by other PIMacs cli modules

;;; Code:

(defun pim-keys-bindings-to-md-refcard (keymapname module)
  "Export the PIMacs key bindings md file.
KEYMAPNAME (string) is the keymap name to export into the file key-bindings-refcard.md of the module MODULE."
  (require 'which-key)
  (let* (
         (which-key-max-description-length 1000)
         ;; (which-key-show-docstrings t)
         ;; (which-key-show-transient-maps 1)
         (keys (which-key--get-bindings nil (symbol-value (intern keymapname)) nil t))
         (line nil)
         (fname (concat (doom-module-locate-path :pimacs module) "/key-bindings-refcard.md"))
         )
    (with-temp-file fname
      (insert (format "# PIMacs %s bindings\n\n" keymapname))
      (insert (format
               "**DO NOT EDIT THIS FILE**, it is auto-generated by the PIMacs cli \"doom +%s pim-to-refcard\"\n\n"
               (symbol-name module)
               ))
      (dolist (key keys)
        (setq line (apply #'format "* **%s**%s%s" key))
        ;; (print! line)
        (when (string-match ".*#pim *$" line)
          (insert (concat line "\n")))
        ))
    (print! (green (format "Generated file : %s" fname)))
    )
  )

(provide 'pimacs/cli)
;;; cli.el ends here

;; Local variables:
;; coding: utf-8
;; End:
