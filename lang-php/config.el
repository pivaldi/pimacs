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

(use-package! phpactor
  :defer t
  :init
  (let ((d (format "%s%s" doom-cache-dir "phpactor")))
    (unless (file-directory-p d)
      (make-directory d))))

;; (use-package! php-mode
;;   :defer t
;;   :hook ((php-mode . (lambda ()
;;                        (set (make-local-variable 'company-backends)
;;                             '(;; list of backends
;;                               company-phpactor
;;                               company-files
;;                               ))))

;;          ))

;; (after! php-mode
;;   (map!
;;    :map php-mode-map
;;    :desc "Execute Phpactor RPC goto_definition command. #pim" "M-." #'phpactor-goto-definition
;;    :desc "Execute Phpactor RPC references action to find references. #pim" "M-?" #'phpactor-find-references
;;    :desc "Open the Transient Php menu. #pim" "C-t" #'transient-php-menu)
;;   )



;; TODO : key bindings collision whit M-<tab> calls php-complete-function :
;; perform function completion on the text around point.

(after! transient
  (transient-define-prefix transient-php-menu ()
    "Phpactor Commands"
    [["Class"
      ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("cn"  "Namespace" phpactor-fix-namespace)]
     ["Properties"
      ("pa"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("pr" "Rename var locally" phpactor-rename-variable-local)
      ("pR" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract"
      ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods"
      ("mi" "Implement Contracts" phpactor-implement-contracts)
      ("mm"  "Generate method" phpactor-generate-method)]
     ["Navigate"
      ("nx" "List refs" phpactor-list-references)
      ("nX" "Replace refs" phpactor-replace-references)
      ("n."  "Goto def" phpactor-goto-definition)]
     ["Phpactor"
      ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)]]))

(load! "+dependencies")

(provide 'pimacs/lang-php)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/config.el")
;; End:
