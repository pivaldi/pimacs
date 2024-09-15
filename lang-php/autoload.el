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

(defun pim--php-get-element (re-pattern)
  "Return forward from the beginig of the buffer the matched element by
RE-PATTERN."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward re-pattern nil t)
        (match-string-no-properties 1)))))

(defun pim--copy-to-kill-ring-maybe (what to-copy)
  "Internal use…"
  (if to-copy
      (progn
        (kill-new to-copy)
        (message "%s '%s' copied." what to-copy))
    (message "%s not found" what)))

(defun pim--insert-maybe (to-insert)
  "Internal use…"
  (if to-insert
      (insert to-insert)
    (message "Nothing to insert")))

;;;###autoload
(defun pim-php-insert-assoc-arrow nil
  "Insert a => arrow."
  (interactive)
  (let ((sp (if (= 32 (char-before)) "" " ")))
    (insert (concat sp "=> "))))

;;;###autoload
(defun pim-php-get-classname-from-buffer ()
  "Get the first class name (without namespace) of the current buffer.
Php standard is to put one class by file !!"
  (interactive)
  (require 'php-mode)
  (pim--php-get-element php--re-classlike-pattern))

;;;###autoload
(defun pim-php-copy-classname-from-buffer ()
  "Copy the class name. See `pim-php-get-classname-from-buffer'"
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php class name" (pim-php-get-classname-from-buffer)))

;;;###autoload
(defun pim-php-get-classname-from-filename (&optional filename)
  "Get the class name from his filename.
The class name of the file /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php
should be \"Zone\" from Php coding guidelines.
Related to `pim-get-filename-uc-part'."
  (interactive "FFile name :")
  (let ((class-ns (pim-get-filename-uc-part filename 1 t))
        (ns (pim-get-filename-uc-part filename 1 nil)))
    (if (string= class-ns "") ""
      (string-replace class-ns "" ns))))

;;;###autoload
(defun pim-php-copy-classname-from-filename (filename)
  "Copy the class name. See `pim-php-get-classname-from-filename'"
  (interactive "FFile name :")
  (pim--copy-to-kill-ring-maybe
   "Php class name" (pim-php-get-classname-from-filename filename)))

;;;###autoload
(defun pim-php-insert-classname-from-filename (&optional filename)
  "Insert the class name from his filename.
See `pim-php-get-classname-from-filename'."
  (interactive "FFile name :")
  (pim--insert-maybe (pim-php-get-classname-from-filename filename)))

;;;###autoload
(defun pim-php-get-namespace-from-buffer ()
  "Get the namespace of the file following the Php guideline."
  (interactive)
  (require 'php-mode)
  (let ((nsp (pim--php-get-element php--re-namespace-pattern)))
    (when nsp (concat "\\" nsp))))

;;;###autoload
(defun pim-php-get-namespace-from-filename (&optional filename)
  "Get the namespace name from his filename.
The namespace name of the file /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php
should be \"\\App\\CPro\\Model\\Poi\\\" following Php coding guidelines.
Related to `pim-get-filename-uc-part'."
  (interactive "FFile name :")
  (let ((ucpart (pim-get-filename-uc-part filename 0 t)))
    (string-replace "/" "\\" (s-chop-suffix "/" ucpart))))

;;;###autoload
(defun pim-php-copy-namespace-from-buffer ()
  "Copy the current namespace if cursor in namespace context.
See `pim-php-get-namespace-from-buffer'."
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php namespace" (pim-php-get-namespace-from-buffer)))

;;;###autoload
(defun pim-php-copy-namespace-from-filename (&optional filename)
  "Copy the namespace name from his filename.
See `pim-php-get-namespace-from-filename'."
  (interactive "FFile name :")
  (pim--copy-to-kill-ring-maybe
   "Php namespace" (pim-php-get-namespace-from-filename filename)))

;;;###autoload
(defun pim-php-insert-namespace-from-filename (&optional filename)
  "Insert the namespace from his filename.
See `pim-php-get-namespace-from-filename'."
  (interactive "FFile name :")
  (pim--insert-maybe (pim-php-get-namespace-from-filename filename)))

;;;###autoload
(defun pim-php-get-class-from-buffer ()
  "Get the first class with namespace of the current buffer.
Php standard is to put one class by file !!"
  (require 'php-mode)
  (let* ((classname (pim-php-get-classname-from-buffer))
         (nsp (pim-php-get-namespace-from-buffer))
         (sep (if nsp "\\" "")))
    (when classname
      (concat (or nsp "") sep classname))))

;;;###autoload
(defun pim-php-copy-class-from-buffer ()
  "Copy the class name. See `pim-php-get-classname-from-buffer'"
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php class" (pim-php-get-class-from-buffer)))


;;;###autoload
(defun pim-php-get-fqsen ()
  "Get class/method FQSEN.
Inspired by the php-mode code source."
  (interactive)
  (require 'php-mode)
  (let ((namespace (or (pim-php-get-namespace-from-buffer) ""))
        (class     (or (pim-php-get-classname-from-buffer) ""))
        (namedfunc (php-get-current-element php-beginning-of-defun-regexp)))
    (concat (if (string= namespace "") "" namespace)
            (if (string= class "") "" (concat "\\" class "::"))
            (if (string= namedfunc "") "" namedfunc ))))

;;;###autoload
(defun pim-php-copy-fqsen ()
  "Copy class/method FQSEN."
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php FQSEN" (pim-php-get-fqsen)))

;;;###autoload
(defun pim-php-compile-file (&optional file lint)
  "Compile the file FILE with linting if LINT not nil."
  (interactive "fPhp file to compile : \nP*")
  (let ((file (or file (buffer-file-name)))
        (option (if lint "-l " "")))
    (compile (format "php %s%s" option file))))

(provide 'pimacs/lang-php/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/autoload.el")
;; End:
