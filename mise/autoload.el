;;; pimacs/mise/autoload.el -*- lexical-binding: t; -*-
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
;; Autoloaded functions for mise integration.

;;; Code:

;;;###autoload
(defun pim-mise-ensure-tools (mise-toml-dir)
  "Run 'mise install' in MISE-TOML-DIR if mise is available.
Returns t if tools were installed successfully, nil otherwise."
  (when (and (executable-find "mise")
             (file-exists-p (expand-file-name "mise.toml" mise-toml-dir)))
    (message "PIMacs: Installing tools from %s/mise.toml..." mise-toml-dir)
    (let ((default-directory mise-toml-dir))
      (= 0 (call-process "mise" nil nil nil "install")))))

(provide 'pimacs/mise/autoload)
;;; autoload.el ends here
