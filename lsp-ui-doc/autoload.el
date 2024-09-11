;;; pimacs/lsp-ui-doc/autoload.el -*- lexical-binding: t; -*-
;;; Package pimacs/keys --- PIMacs key binding -*- lexical-binding: t; -*-
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


;;;###autoload
(defun pim-toggle-lsp-ui-doc-show-with-cursor ()
  "Toogle lsp ui doc show with cursor."
  (interactive)
  (when lsp-ui-doc-show-with-cursor (lsp-ui-doc-hide))
  (setq lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor)))

(provide 'pimacs/lsp-ui-doc/autoload)
;; autoload.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lsp-ui-doc/autoload.el")
;; End:
