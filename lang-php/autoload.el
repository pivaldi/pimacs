;;; pimacs/lang-php/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-insert-php-assoc-arrow nil
  "Insert a => arrow."
  (interactive)
  (let ((sp (if (= 32 (char-before)) "" " ")))
    (insert (concat sp "=> "))))
