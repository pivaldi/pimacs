;;; pimacs/lang-php/config.el -*- lexical-binding: t; -*-

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

(provide 'pimacs/lang-php)
;;; config.el ends here

(use-package! phpactor
  :defer t
  :init
  (let ((d (format "%s%s" doom-cache-dir "phpactor")))
    (unless (file-directory-p d)
      (make-directory d))))


(after! php-mode
  (map!
   :map php-mode-map
   :desc "Execute Phpactor RPC goto_definition command. #pim" "M-." #'phpactor-goto-definition
   :desc "Execute Phpactor RPC references action to find references. #pim" "M-?" #'phpactor-find-references
   :desc "Open the Transient Php menu. #pim" "C-t" #'transient-php-menu)
  )

(after! transient
  (transient-define-prefix transient-php-menu ()
    "Phpactor Commands"
    [["Class"
      ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("n"  "Namespace" phpactor-fix-namespace)]
     ["Properties"
      ("a"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("r" "Rename var locally" phpactor-rename-variable-local)
      ("R" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract"
      ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods"
      ("i" "Implement Contracts" phpactor-implement-contracts)
      ("m"  "Generate method" phpactor-generate-method)]
     ["Navigate"
      ("x" "List refs" phpactor-list-references)
      ("X" "Replace refs" phpactor-replace-references)
      ("."  "Goto def" phpactor-goto-definition)]
     ["Phpactor"
      ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)]]))


;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/config.el")
;; End:
