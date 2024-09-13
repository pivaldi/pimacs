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
(defun pim-insert-php-assoc-arrow nil
  "Insert a => arrow."
  (interactive)
  (let ((sp (if (= 32 (char-before)) "" " ")))
    (insert (concat sp "=> "))))

;;;###autoload
(defun pim-get-php-classname-from-buffer ()
  "Get the first class name (without namespace) of the current buffer.
Php standard is to put one class by file !!"
  (interactive)
  (require 'php-mode)
  (pim--get-php-element php--re-classlike-pattern))

;;;###autoload
(defun pim-copy-php-classname-from-buffer ()
  "Copy the class name. See `pim-get-php-classname-from-buffer'"
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php class name" (pim-get-php-classname-from-buffer)))

(defun pim--get-filename-uc-part (&optional filename offset trim-file-name)
  "Get the longest terminating first upper case part of a file name.
- If FILENAME is missing, use the filename attached to the current buffer if
  any.
- If OFFSET an integer is not nil, truncates the first characters by OFFSET
  chars.
- If TRIM-FILE-NAME an bool is not nil, remove the filename part.

Example :
If FILENAME is /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php the function
will return :
- /App/CPro/Model/Poi/Zone without option
- App/CPro/Model/Poi/Zone with option OFFSET 1
- /App/CPro/Model/Poi/ with option TRIM-FILE-NAME t"
  (let* ((offset (or offset 0))
         (filename (file-name-sans-extension (or filename (buffer-file-name))))
         (filename (if trim-file-name
                       (file-name-directory filename) filename)))
    (substring filename
               (+ offset
                  (let ((case-fold-search nil))
                    (or (string-match
                         "\\\(/[A-Z][a-zA-Z0-9.-_]+\\\)+$" filename)
                        (- (length filename) offset)))))))

;;;###autoload
(defun pim-get-php-classname-from-filename (&optional filename)
  "Get the class name from his filename.
The class name of the file /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php
should be \"Zone\" from Php coding guidelines.
Related to `pim--get-filename-uc-part'."
  (interactive "FFile name :")
  (let ((class-ns (pim--get-filename-uc-part filename 1 t))
        (ns (pim--get-filename-uc-part filename 1 nil)))
    (if (string= class-ns "") ""
      (string-replace class-ns "" ns))))

;;;###autoload
(defun pim-copy-php-classname-from-filename (filename)
  "Copy the class name. See `pim-get-php-classname-from-filename'"
  (interactive "FFile name :")
  (pim--copy-to-kill-ring-maybe
   "Php class name" (pim-get-php-classname-from-filename filename)))

;;;###autoload
(defun pim-insert-php-classname-from-filename (&optional filename)
  "Insert the class name from his filename.
See `pim-get-php-classname-from-filename'."
  (interactive "FFile name :")
  (pim--insert-maybe (pim-get-php-classname-from-filename filename)))

;;;###autoload
(defun pim-get-php-namespace-from-buffer ()
  "Get the namespace of the file following the Php guideline."
  (interactive)
  (require 'php-mode)
  (let ((nsp (pim--get-php-element php--re-namespace-pattern)))
    (when nsp (concat "\\" nsp))))

;;;###autoload
(defun pim-get-php-namespace-from-filename (&optional filename)
  "Get the namespace name from his filename.
The namespace name of the file /var/www/xxx/yyyy/zzz/App/CPro/Model/Poi/Zone.php
should be \"\\App\\CPro\\Model\\Poi\\\" following Php coding guidelines.
Related to `pim--get-filename-uc-part'."
  (interactive "FFile name :")
  (let ((ucpart (pim--get-filename-uc-part filename 0 t)))
    (string-replace "/" "\\" (s-chop-suffix "/" ucpart))))

;;;###autoload
(defun pim-copy-php-namespace-from-buffer ()
  "Copy the current namespace if cursor in namespace context.
See `pim-get-php-namespace-from-buffer'."
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php namespace" (pim-get-php-namespace-from-buffer)))

;;;###autoload
(defun pim-copy-php-namespace-from-filename (&optional filename)
  "Copy the namespace name from his filename.
See `pim-get-php-namespace-from-filename'."
  (interactive "FFile name :")
  (pim--copy-to-kill-ring-maybe
   "Php namespace" (pim-get-php-namespace-from-filename filename)))

;;;###autoload
(defun pim-insert-php-namespace-from-filename (&optional filename)
  "Insert the namespace from his filename.
See `pim-get-php-namespace-from-filename'."
  (interactive "FFile name :")
  (pim--insert-maybe (pim-get-php-namespace-from-filename filename)))

;;;###autoload
(defun pim-get-php-class-from-buffer ()
  "Get the first class with namespace of the current buffer.
Php standard is to put one class by file !!"
  (require 'php-mode)
  (let* ((classname (pim-get-php-classname-from-buffer))
         (nsp (pim-get-php-namespace-from-buffer))
         (sep (if nsp "\\" "")))
    (when classname
      (concat (or nsp "") sep classname))))

;;;###autoload
(defun pim-copy-php-class-from-buffer ()
  "Copy the class name. See `pim-get-php-classname-from-buffer'"
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php class" (pim-get-php-class-from-buffer)))


;;;###autoload
(defun pim-get-php-fqsen ()
  "Get class/method FQSEN.
Inspired by the php-mode code source."
  (interactive)
  (require 'php-mode)
  (let ((namespace (or (pim-get-php-namespace-from-buffer) ""))
        (class     (or (pim-get-php-classname-from-buffer) ""))
        (namedfunc (php-get-current-element php-beginning-of-defun-regexp)))
    (concat (if (string= namespace "") "" namespace)
            (if (string= class "") "" (concat "\\" class "::"))
            (if (string= namedfunc "") "" namedfunc ))))

;;;###autoload
(defun pim-copy-php-fqsen ()
  "Copy class/method FQSEN."
  (interactive)
  (pim--copy-to-kill-ring-maybe
   "Php FQSEN" (pim-get-php-fqsen)))

(provide 'pimacs/lang-php/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/autoload.el")
;; End:
