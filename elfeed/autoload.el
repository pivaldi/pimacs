;;; pimacs/elfeed/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-elfeed-tag-sort (a b)
  "Custom elfeed sort function based on serialization tag list into
a string, falling back on the date to break ties (descending).
See https://github.com/skeeto/elfeed/issues/203#issuecomment-335038206"
  (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
         (b-tags (format "%s" (elfeed-entry-tags b))))
    (if (string= a-tags b-tags)
        (< (elfeed-entry-date b) (elfeed-entry-date a)))
    (string< a-tags b-tags)))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/elfeed/autoload.el")
;; End:
