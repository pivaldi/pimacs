;;; pimacs/yasnippet/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-basic-snippet-expand-condition ()
  "Used in pimacs yas/snippet.
Does not expand if the snippet is folowed or preceded by a letter"
  (let ((chara (char-to-string (char-after))))
    (not (string-match "[a-zA-Z]" chara))))
