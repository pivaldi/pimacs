;;; pimacs/dired/config.el -*- lexical-binding: t; -*-
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

(doom!
 :emacs
 dired)

(use-package! dired
  :config
  (map!
   :map dired-mode-map
   :desc "Use `wdired-change-to-wdired-mode` if multiple files are marked otherwise use `dired-do-rename`. #pim"
   "R" #'pim/dired-rename))

(when (featurep :system 'linux)
  (after! dired
    ;; This lib comes from https://github.com/kickingvegas/cclisp/blob/e5397b5b08d9b96633a2cf13b230e71e02697b3f/cc-dired-sort-by.el
    (load! "+cc-dired-sort-by")

    (defun pim/dired-sort-by (&optional arg)
      "Dired sort by `cc/dired-sort-by' or, if ARG is not nil, by native `dired-sort-toggle-or-edit'."
      (interactive "P")
      (if arg (if (equal arg '(4)) (dired-sort-toggle-or-edit) (dired-sort-toggle-or-edit 4)) (cc/dired-sort-by)))

    (map!
     :map dired-mode-map
     :desc "Sort with Charles Choi fancy sort or the native Dired sort if prefix or double prefix. #pim" "s" #'pim/dired-sort-by)))

(provide 'pimacs/dired)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/dired/config.el")
;; End:
