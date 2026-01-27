;;; pimacs/lang-go/config.el -*- lexical-binding: t; -*-
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

(setenv "GO111MODULE" "on")

;; Auto-install tools if mise module is available
(when (modulep! :pimacs mise)
  (pim-mise-ensure-tools (file-name-directory load-file-name)))

(if (not (executable-find "goimports"))
    (add-to-list 'pim-error-msgs "Please install goimports : https://godoc.org/golang.org/x/tools/cmd/goimports"))
(if (not (executable-find "gofmt"))
    (add-to-list 'pim-error-msgs "Please install gofmt : https://godoc.org/golang.org/x/tools/cmd/gofmt"))
(if (not (executable-find "godef"))
    (add-to-list 'pim-error-msgs "Please install godef : go install github.com/rogpeppe/godef@latest"))
(if (not (executable-find "gocode"))
    (add-to-list 'pim-error-msgs "Please install gocode : go install github.com/nsf/gocode@latest"))

(use-package! flycheck-golangci-lint
  :defer t
  :ensure t ;; We need it !!
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :config
  (unless  (executable-find "golangci-lint")
    (add-to-list 'pim-error-msgs
                 "Please install golangci-lint <https://github.com/golangci/golangci-lint>")))

(load! "+go-mode") ;; go-mode by default

;; Load go-ts-mode if available
(when (fboundp 'go-ts-mode)
  ;; (or (> emacs-major-version 29) (and (= emacs-major-version 29) (>= emacs-minor-version 1)))
  (load! "+go-ts-mode"))

(after! lsp
  (setq lsp-disabled-clients '((go-mode . (semgrep-ls)) (go-ts-mode . (semgrep-ls)))))

(provide 'pimacs/lang-go/config)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/config.el")
;; End:
