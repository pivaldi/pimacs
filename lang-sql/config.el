;;; pimacs/lang-sql/config.el -*- lexical-binding: t; -*-
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

(when (and (modulep! treesit)
           (fboundp 'treesit-install-language-grammar))
  (add-to-list 'treesit-language-source-alist
               '((sql . ("https://github.com/DerekStride/tree-sitter-sql.git" "gh-pages" "src"))))
  (unless (and (treesit-language-available-p 'sql) (treesit-ready-p 'sql))
    (treesit-install-language-grammar 'sql)
    )

  font-lock-keyword-face
  )


(provide 'pimacs/lang-sql)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-sql/config.el")
;; End:
