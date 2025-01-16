;;; pimacs/crypt/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-hide-password ()
  "Use `authinfo--hide-passwords' to hide password in the current buffer."
  (interactive)
  (authinfo--hide-passwords (point-min) (point-max))
  (reveal-mode))

(defcustom pim-gen-password-length 12
  "The default length of password `pim-gen-password'"
  :type 'integer
  :group `pimacs)

;;;###autoload
(defun pim-gen-password (&optional len)
  "Generate a good password.

Default length is `pim-gen-password' but this can be overwrite
by LEN or with prefix arguments."
  (interactive "P")
  (password-generator-paranoid (or len pim-gen-password-length)))
