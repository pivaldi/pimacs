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

(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript")))

  (unless (pim--js-ts-mode-available-p)
    ;; Try to install missing parser
    (condition-case err
        (progn
          (message "javascript tree-sitter parser not installed. PIMacs is installing it for you...")
          (treesit-install-language-grammar 'javascript))
      (error
       (add-to-list 'pim-error-msgs
                    (format "Failed to install javascript tree-sitter grammar: %s" (error-message-string err))))))

  ;; Check again after installation attempt
  (if (pim--js-ts-mode-available-p)
      (progn
        ;; Remap js-mode to js-ts-mode
        (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
        (pim--js-map t))
    ;; Still not available, fall back to js-mode
    (add-to-list 'pim-error-msgs
                 "js-ts-mode parser unavailable. Falling back to js-mode.")
    (pim--js-map nil)))

(provide 'pimacs/lang-js/+treesit)
;;; +treesit.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-js/+treesit.el")
;; End:
