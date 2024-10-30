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
  :init
  (add-hook 'go-mode-hook #'pim-go_ts_mode-hook)
  :config
  (setq gofmt-command "goimports")
  (setq gofmt-args nil)
  (when (executable-find "goimports")
    (setq-hook! 'go-mode-hook +format-with-lsp nil)
    (with-eval-after-load 'apheleia
      (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports))))
  (if (modulep! +lsp)
      (progn
        (after! lsp
          (add-to-list 'lsp-disabled-clients '((go-mode . (semgrep-ls))))) ;; Useless lsp-client for Go
        (add-hook 'go-mode-hook #'lsp-deferred)
        (map!
         :map go-mode-map
         :desc "Find definitions of the symbol under point with LSP. #pim" "M-." #'lsp-find-definition
         :desc "Find references of the symbol under point with LSP. #pim" "M-?" #'lsp-find-references)
        )
    (progn
      (map!
       :map go-mode-map
       :desc "Find the definition of the identifier at point with xref. #pim" "M-." #'xref-find-definitions
       :desc "Find references to the identifier at point with xref. #pim" "M-?" #'xref-find-references
       :desc "Go back to the previous position in xref history.. #pim" "M-," #'xref-go-back
       ))
    )

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
