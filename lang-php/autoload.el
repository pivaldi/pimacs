;;; pimacs/lang-php/autoload.el -*- lexical-binding: t; -*-
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

(defun pim--get-php-element (re-pattern)
  "Return forward from the beginig of the buffer the matched element by
RE-PATTERN."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward re-pattern nil t)
        (match-string-no-properties 1)))))

;;;###autoload
(defun pim-insert-php-assoc-arrow nil
  "Insert a => arrow."
  (interactive)
  (let ((sp (if (= 32 (char-before)) "" " ")))
    (insert (concat sp "=> "))))

;;;###autoload
(defun pim-get-php-class-name ()
  "Get the first class name (without namespace) of the current buffer.
Php standard is to put one class by file !!"
  (interactive)
  (require 'php-mode)
  (pim--get-php-element php--re-classlike-pattern))
)))

;;;###autoload
(defun pim-copy-php-class-name ()
  "Copy the class name. See `pim-get-php-class-name'"
  (interactive)
  (let ((class (pim-get-php-class-name)))
    (if class
        (progn
          (kill-new class)
          (message "Php class '%s' copied." class))
      (progn
        (message "Php class not found…")))))

;;;###autoload
(defun pim-get-php-namespace ()
  "Get the namespace of the file (Php guideline)."
  (interactive)
  (require 'php-mode)
  (let ((nsp (pim--get-php-element php--re-namespace-pattern)))
    (when nsp (concat "\\" nsp))))

;;;###autoload
(defun pim-copy-php-namespace ()
  "Copy the current namespace if cursor in namespace context."
  (interactive)
  (let ((nsp (pim-get-php-namespace)))
    (if nsp
        (progn
          (kill-new nsp)
          (message "Php namespace '%s' copied." nsp))
      (message "Php namespace not found"))))

;;;###autoload
(defun pim-get-php-fqsen ()
  "Get class/method FQSEN.
Inspired by the php-mode code source."
  (interactive)
  (require 'php-mode)
  (let ((namespace (or (pim--get-php-element php--re-namespace-pattern) ""))
        (class     (or (pim--get-php-element php--re-classlike-pattern) ""))
        (namedfunc (pim--get-php-element php-beginning-of-defun-regexp)))
    (concat (if (string= namespace "") "" namespace)
            (if (string= class "") "" (concat "\\" class "::"))
            (if (string= namedfunc "") "" namedfunc ))))

(provide 'pimacs/lang-php/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/autoload.el")
;; End:
