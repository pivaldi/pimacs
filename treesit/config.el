;;; Package pimacs/treesit --- PIMacs tree-sitter config
;;; pimacs/treesit/config.el -*- lexical-binding: t; -*-
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

(when (< emacs-major-version 29)
  (add-to-list 'pim-error-msgs "Emacs 29+ is needed by treesit module"))

(unless (fboundp 'treesit-install-language-grammar)
  (add-to-list
   'pim-error-msgs
   "Native treesit module *no*t found.  Complie Emacs with configuration option --with-tree-sitter"))

(use-package! treesit
  :defer t
  :config
  (setq treesit-font-lock-level 4) ;; Maximum treesit font decoration
  )

(provide 'pimacs/treesit)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/treesit/config.el")
;; End:
