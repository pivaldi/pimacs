;;; pimacs/lang-go/autoload.el -*- lexical-binding: t; -*-
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
(defun pim-go-ts-mode-hook nil
  "`go-ts-mode' and `go-mode' common hook."
  (setq tab-width 2)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist '((?` _ ?`)))
  )

;;;###autoload
(defun pim-flycheck-golangci-lint-ts-setup-maybe ()
  "Setup Flycheck GolangCI-Lint if featured.
Add `pim-golangci-lint-ts' for `go-ts-mode' to `flycheck-checkers'."
  (interactive)
  (when (and (featurep 'flycheck) (featurep 'flycheck-golangci-lint))
    (add-to-list 'flycheck-checkers 'pim-golangci-lint-ts))
  )

;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/autoload.el")
;; End:
