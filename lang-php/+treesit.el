;;; pimacs/lang-php/+treesit.el -*- lexical-binding: t; -*-
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

;; Native Tree Sitter config for php.

;;; Code:

(after! treesit
  (if (< emacs-major-version 30)
      (progn  ;; emacs < 30
        (add-to-list 'treesit-language-source-alist
                     '((php . ("https://github.com/tree-sitter/tree-sitter-php.git" "master" "php/src"))))

        (unless (and (treesit-language-available-p 'php) (treesit-ready-p 'php))
          ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
          ;; (unless (treesit-ready-p 'php)
          (treesit-install-language-grammar 'php))
        (when (treesit-ready-p 'php)
          (add-hook
           'php-mode-hook
           (lambda nil
             (interactive)
             "Add tree sitter parser in the buffer"
             (treesit-parser-create 'php)
             ))))
    (progn  ;; emacs >= 30
      (unless (and
               (treesit-ready-p 'php)
               (treesit-ready-p 'phpdoc)
               (treesit-ready-p 'html)
               (treesit-ready-p 'javascript)
               (treesit-ready-p 'jsdoc)
               (treesit-ready-p 'css))
        (warn "php tree sitter parser are not installed. PIMacs is installing they for you…")
        (php-ts-mode-install-parsers))
      (pim--php-map t))))

(provide 'pimacs/lang-php/+treesit)
;;; +treesit.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/+treesit.el")
;; End:
