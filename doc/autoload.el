;;; pimacs/doc/autoload.el -*- lexical-binding: t; -*-
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

(defun pim-keystring-kbd-consolidate (keystr)
  "Consolidate KEYSTR, a string key representation, to be read by `kbd'."
  (string-replace
   ">>" ">"
   (string-replace
    "<<" "<"
    (s-replace-regexp "\\(\[a-zA-Z0-9]\\{2,\\}\\)" "<\\1>" keystr)))
  )

(defun pim-which-key-insert-bindings-recursively-org (prefix level)
  "Insert recursively all key bindings with prefix PREFIX with org formating.
LEVEL represent the deep of the heading."
  (require 'which-key)
  (let* ((which-key-max-description-length 1000)
         (keys (which-key--get-bindings (kbd prefix)))
         (section-s (make-string level ?*)))
    (dolist (key keys)
      (let ((prefixn (s-trim (format "%s %s" prefix (pim-keystring-kbd-consolidate (pop key))))))
        (insert (apply #'format "%s =%s=%s%s\n\n" section-s prefixn key))
        (pim-which-key-insert-bindings-recursively-org prefixn (+ 1 level))
        )
      )
    )
  )

;;;###autoload
(defun pim/which-key-export-bindings-recursively-to-file (prefix file-name)
  "Export bindings recursively from PREFIX into file FILE-NAME with org formatting.
PREFIX should be a string suitable for `kbd'."
  (interactive "sPrefix: \nF")
  (require 'toc-org)
  (with-temp-file file-name
    (point-max)
    (save-excursion
      (if (equal prefix "")
          (insert "#+title: All Fundamental Key Bindings\n\n")
        (insert (format "#+title: Fundamental Key Bindings Prefixed by %s\n\n" prefix)))
      (insert "* Table of Content :TOC_1:\n\n")
      (pim-which-key-insert-bindings-recursively-org prefix 1)
      (toc-org-insert-toc))))

;;;###autoload
(defun pim-generate-all-fundamental-key-bindings ()
  "PIMacs internal use only. User should use `pim/which-key-export-bindings-recursively-to-file' instead."
  (let
      ((fname "")
       (prefixes pim-doc-key-binding-prefixes-to-export))
    (dolist (prefix prefixes)
      (setq fname (format "%s/doom-refcard-%s.org" (doom-module-locate-path :pimacs 'doc) prefix))
      (pim/which-key-export-bindings-recursively-to-file prefix fname))))

(provide 'pimacs/doc/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/doc/autoload.el")
;; End:
