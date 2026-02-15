;;; pimacs/lang-protobuf/autoload.el -*- lexical-binding: t; -*-
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

;;;###autoload
(defun pim-protobuf-tools-install-globally (&optional trusted)
  "Install globally Protobuf tools if pimacs/mise module is available.
If TRUSTED is t, auto-trust the mise.toml file else ask to the user."
  (interactive)
  (if (modulep! :pimacs mise)
      (let ((installed (pim-mise-install (doom-module-expand-path '(:pimacs . lang-protobuf)) trusted)))
        (unless (executable-find "buf")
          (pim-mise-use-global "buf@latest"))
        (message (if installed "done" "failed")))
    (warn "pimacs/mise is not loaded so Protobuf tools can not be installed.")))

(provide 'pimacs/lang-protobuf/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-protobuf/autoload.el")
;; End:
