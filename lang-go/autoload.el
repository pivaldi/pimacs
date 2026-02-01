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
(defun pim-go-tools-install-globally (&optional trusted)
  "Install globally Go tools if pimacs/mise module is available.
If TRUSTED is t, auto-trust the mise.toml file else ask to the user."
  (interactive)
  (if (modulep! :pimacs mise)
      (let ((installed (pim-mise-install (doom-module-expand-path '(:pimacs . lang-go)) trusted)))
        (unless (executable-find "go")
          (pim-mise-use-global "go@latest"))
        (unless (executable-find "golangci-lint")
          (pim-mise-use-global "golangci-lint@latest"))
        (message (if installed "done" "failed")))
    (warn "pimacs/mise is not loaded so Go tools can not be installed.")))

;;;###autoload
(defun pim-go-ts-mode-hook nil
  "`go-ts-mode' and `go-mode' common hook."
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

;;;###autoload
(defun pim-go-run ()
  "Run current buffer file name with \"go run\""
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(provide 'pimacs/lang-go/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/autoload.el")
;; End:
