;;; pimacs/lang-go/+go-mode.el -*- lexical-binding: t; -*-
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

(unless (modulep! :lang go)
  (if (modulep! +lsp)
      (doom! :lang go +lsp)
    (doom! :lang go)))

(use-package! go-mode
  :defer t
  :hook
  (go-mode . (pim-go_ts_mode-hook))
  :init
  (when (modulep! +lsp)
    (add-hook 'go-mode-hook #'lsp-deferred))
  (map!
   :map go-mode-map
   :desc "Jump to the definition of the expression at POINT. #pim" "M-." #'godef-jump)
  )


(use-package! flycheck-golangci-lint
  :ensure t ;; We need it !!
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :config
  (unless  (executable-find "golangci-lint")
    (add-to-list 'pim-error-msgs
                 "Please install golangci-lint <https://github.com/golangci/golangci-lint>"))
  )

;; Enable golangci-lint in go-mode with fix for lsp-mode
(after! (go-mode flycheck-golangci-lint pimacs/lsp)
  (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup)
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                (setq pim--flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))))

;;; +go-mode.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/+go-mode.el")
;; End:
