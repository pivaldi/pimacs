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

;; (remove-hook! (pim-go_ts_mode-hook . go-ts-mode-hook))
;; (remove-hook! (pim-flycheck-golangci-lint-ts-setup-maybe . go-ts-mode-hook))

(use-package! go-ts-mode
  ;; :defer t
  ;; :hook
  ;; (go-ts-mode . (pim-go_ts_mode-hook pim-flycheck-golangci-lint-ts-setup-maybe))
  ;; (go-ts-mode . (pim-flycheck-golangci-lint-ts-setup-maybe))

  :init
  ;; (when (modulep! +lsp)
  ;;   (add-hook 'go-ts-mode-hook #'lsp-deferred))
  (add-hook 'go-ts-mode-hook #'pim-go_ts_mode-hook)
  ;; (add-hook 'go-ts-mode-hook #'pim-flycheck-golangci-lint-ts-setup-maybe)

  ;; :init
  ;; (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(go-mode-mode . go-mod-ts-mode))

  :config
  (map!
   :map go-ts-mode-map
   :desc "Jump to the definition of the expression at POINT. #pim" "M-." #'godef-jump)
  )

(use-package! flycheck-golangci-lint
  ;; :defer t
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :config
  (unless  (executable-find "golangci-lint")
    (add-to-list 'pim-error-msgs
                 "Please install golangci-lint <https://github.com/golangci/golangci-lint>"))
  )

(after! (go-ts-mode flycheck-golangci-lint)
  (flycheck-define-checker pim-golangci-lint-ts
    "A Go syntax checker using golangci-lint for go-ts-mode."
    :command ("golangci-lint" "run" "--out-format=checkstyle"
              (option "--config=" flycheck-golangci-lint-config concat)
              (option "--timeout=" flycheck-golangci-lint-deadline concat)
              (option-flag "--tests" flycheck-golangci-lint-tests)
              (option-flag "--fast" flycheck-golangci-lint-fast)
              (option-flag "--allow-parallel-runners" flycheck-golangci-allow-parallel-runners)
              (option-flag "--allow-serial-runners" flycheck-golangci-allow-serial-runners)
              (option-flag "--disable-all" flycheck-golangci-lint-disable-all)
              (option-flag "--enable-all" flycheck-golangci-lint-enable-all)
              (option-list "--disable=" flycheck-golangci-lint-disable-linters concat)
              (option-list "--enable=" flycheck-golangci-lint-enable-linters concat)
              ".")
    :error-parser flycheck-parse-checkstyle
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ":" line ":" (message) line-end))
    :modes go-ts-mode)

  ;; (after! pimacs/lsp
  ;;   (add-hook 'lsp-managed-mode-hook
  ;;             (lambda ()
  ;;               (when (derived-mode-p 'go-ts-mode)
  ;;                 (setq pim--flycheck-local-checkers '((lsp . ((next-checkers . (pim-golangci-lint-ts))))))))))
  (add-hook 'flycheck-mode-hook #'pim-flycheck-golangci-lint-ts-setup-maybe)
  )

;;; +go-ts-mode.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/+go-ts-mode.el")
;; End:
