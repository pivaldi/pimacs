;;; pimacs/crypt --- Encrypting/decrypting  -*- lexical-binding: t; -*-
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

(use-package! epa
  :init
  ;; This enables the automatic encryption and decryption of files.
  (epa-file-enable)
  )

(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p ".*\.gpg~?" (file-name-nondirectory (buffer-file-name)))
              (pimacs-hide-password)
              )))


(provide 'pimacs/crypt)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/crypt/config.el")
;; End:
