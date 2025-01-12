;;; pimacs/crypt/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun pimacs-hide-password ()
  "Use authinfo--hide-passwords to hide password in file."
  (interactive)
  (authinfo--hide-passwords (point-min) (point-max))
  (reveal-mode))
