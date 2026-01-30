;;; pimacs/lang-go/+go-ts-mode.el -*- lexical-binding: t; -*-

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

(when (or (modulep! +lsp) (modulep! :pimacs lsp))
  (add-hook 'go-ts-mode-hook #'lsp-deferred))
(add-hook 'go-ts-mode-hook #'pim-go-ts-mode-hook)

(when (executable-find "goimports")
  (setq-hook! 'go-ts-mode-hook +format-with-lsp nil)
  (with-eval-after-load 'apheleia
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports))))
(setq go-ts-mode-indent-offset tab-width)

(after! go-ts-mode
  (map! :map go-ts-mode-map
        :desc "Run the current buffer filename with \"go run\". #pim" "C-c C-c" #'pim-go-run)


  (if (modulep! +lsp)
      (progn
        (after! lsp
          (add-to-list 'lsp-disabled-clients '((go-ts-mode . (semgrep-ls)))))  ;; Useless lsp-client for Go
        (map!
         :map go-ts-mode-map
         :desc "Find definitions of the symbol under point with LSP. #pim" "M-." #'lsp-find-definition
         :desc "Find references of the symbol under point with LSP. #pim" "M-?" #'lsp-find-references)
        )
    (progn
      (map!
       :map go-ts-mode-map
       :desc "Find the definition of the identifier at point with xref. #pim" "M-." #'xref-find-definitions
       :desc "Find references to the identifier at point with xref. #pim" "M-?" #'xref-find-references
       :desc "Go back to the previous position in xref history.. #pim" "M-," #'xref-go-back
       ))
    ))

(after! (flycheck pimacs/lsp)
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (eq major-mode 'go-ts-mode)
                (setq pim--flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))))

;;; +go-ts-mode.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/+go-ts-mode.el")
;; End:
