;;; pimacs/treemacs/config.el -*- lexical-binding: t; -*-
;; Copyright (c) 2026, Philippe Ivaldi <www.piprime.fr>

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

(doom! :ui treemacs)

(use-package! treemacs
  :init
  (when pim-azertyp (map! :desc "Select/Unselect the treemacs window if it is visible. #pim" "M-à" #'treemacs-select-window))

  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (when (modulep! :tools lsp)
    (lsp-treemacs-sync-mode 1)
    )
  )

(provide 'pimacs/treemacs)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/treemacs/config.el")
;; End:
