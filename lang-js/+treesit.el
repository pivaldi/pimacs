;;; pimacs/lang-js/+treesit.el -*- lexical-binding: t; -*-
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

;; Native Tree Sitter config for JavaScript.

;;; Code:

(defun pim--js-ts-mode-available-p ()
  "Return non-nil if js-ts-mode can be used (javascript parser available)."
  (and (treesit-available-p)
       (treesit-language-available-p 'javascript)))

;; Deferred setup - only runs when opening a JS file
(defvar pim--js-treesit-setup-done nil
  "Non-nil if JavaScript tree-sitter setup has been performed.")

(defun pim--js-treesit-setup ()
  "Setup tree-sitter for JavaScript. Called lazily on first JS file."
  (unless pim--js-treesit-setup-done
    (setq pim--js-treesit-setup-done t)
    (if (pim--js-ts-mode-available-p)
        (progn
          ;; Remap js-mode to js-ts-mode
          (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
          (pim--js-map t))
      ;; Not available, use js-mode
      (pim--js-map nil))))

;; Run setup when js-mode or js-ts-mode is loaded
(after! js (pim--js-treesit-setup))
(after! js-ts-mode (pim--js-treesit-setup))

(provide 'pimacs/lang-js/+treesit)
;;; +treesit.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-js/+treesit.el")
;; End:
