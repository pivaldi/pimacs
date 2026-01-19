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

(defun pim--php-ts-mode-available-p ()
  "Return non-nil if php-ts-mode can be used (all required parsers available)."
  (and (treesit-available-p)
       (treesit-language-available-p 'php)
       (or (< emacs-major-version 30)
           ;; Emacs 30+ php-ts-mode requires additional parsers
           (and (treesit-language-available-p 'phpdoc)
                (treesit-language-available-p 'html)
                (treesit-language-available-p 'javascript)
                (treesit-language-available-p 'jsdoc)
                (treesit-language-available-p 'css)))))

(after! treesit
  (if (< emacs-major-version 30)
      (progn  ;; emacs < 30
        (add-to-list 'treesit-language-source-alist
                     '(php . ("https://github.com/tree-sitter/tree-sitter-php.git" "master" "php/src")))

        (unless (and (treesit-language-available-p 'php) (treesit-ready-p 'php))
          (treesit-install-language-grammar 'php))
        (when (treesit-ready-p 'php)
          (add-hook
           'php-mode-hook
           (lambda nil
             (interactive)
             "Add tree sitter parser in the buffer"
             (treesit-parser-create 'php)))))
    (progn  ;; emacs >= 30
      (unless (pim--php-ts-mode-available-p)
        ;; Try to install missing parsers
        (require 'php-ts-mode nil t)
        (when (fboundp 'php-ts-mode-install-parsers)
          (condition-case err
              (progn
                (message "php-ts-mode parsers not installed. PIMacs is installing them for you…")
                (php-ts-mode-install-parsers))
            (error
             (add-to-list 'pim-error-msgs
                          (format "Failed to install php-ts-mode parsers: %s" (error-message-string err)))))))
      ;; Check again after installation attempt
      (if (pim--php-ts-mode-available-p)
          (pim--php-map t)
        ;; Still not available, fall back to php-mode
        (add-to-list 'pim-error-msgs
                     "php-ts-mode parsers unavailable. Falling back to php-mode.")
        (setq auto-mode-alist (rassq-delete-all 'php-ts-mode auto-mode-alist))
        (setq major-mode-remap-alist (assq-delete-all 'php-mode major-mode-remap-alist))
        (pim--php-map nil)))))

(provide 'pimacs/lang-php/+treesit)
;;; +treesit.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/+treesit.el")
;; End:
