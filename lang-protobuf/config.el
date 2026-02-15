;;; pimacs/lang-protobuf/config.el -*- lexical-binding: t; -*-
;; Copyright (c) 2026, Philippe Ivaldi <www.piprime.fr>

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
;; Configuration for protobuf-ts-mode with buf toolchain integration.

;;; Code:

(unless (executable-find "buf")
  (add-to-list 'pim-error-msgs
               "lang-protobuf: Please install buf (https://buf.build)
If you use pimacs/mise with +tools, restarting emacs is needed."))

(defun pim-buf-lint ()
  "Run `buf lint' in the current project."
  (interactive)
  (compile "buf lint"))

(defun pim-buf-format ()
  "Run `buf format -w' on the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (format "buf format -w %s" (shell-quote-argument file)))
      (revert-buffer t t t))))

(defun pim-buf-build ()
  "Run `buf build' in the current project."
  (interactive)
  (compile "buf build"))

(defun pim-buf-breaking ()
  "Run `buf breaking' against main branch."
  (interactive)
  (compile "buf breaking --against '.git#branch=main'"))

(defun pim-buf-generate ()
  "Run `buf generate' in the current project."
  (interactive)
  (compile "buf generate"))

(defun pim-buf-mod-update ()
  "Run `buf mod update' in the current project."
  (interactive)
  (compile "buf mod update"))

(use-package! protobuf-ts-mode
  :defer t
  :mode "\\.proto\\'"
  :hook (protobuf-ts-mode . pim-protobuf-ts-mode-setup)
  :init
  (after! yasnippet
    (add-to-list 'yas-snippet-dirs
                 (expand-file-name "snippets" (doom-module-expand-path '(:pimacs . lang-protobuf)))))
  :config
  (defun pim-protobuf-ts-mode-setup ()
    "Setup hook for protobuf-ts-mode."
    (add-hook 'before-save-hook #'pim-buf-format-before-save nil t))

  (defun pim-buf-format-before-save ()
    "Format buffer with buf before saving."
    (when (and (executable-find "buf") (buffer-file-name))
      (pim-buf-format)))

  (map! :map protobuf-ts-mode-map
        :localleader
        :desc "Buf Lint. #pim" "l" #'pim-buf-lint
        :desc "Buf Format. #pim" "f" #'pim-buf-format
        :desc "Buf Build. #pim" "b" #'pim-buf-build
        :desc "Buf Breaking. #pim" "B" #'pim-buf-breaking
        :desc "Buf Generate. #pim" "g" #'pim-buf-generate
        :desc "Buf Update deps. #pim" "u" #'pim-buf-mod-update))

(when (modulep! +lsp)
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(protobuf-ts-mode . "protobuf"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("buf" "lsp"))
      :activation-fn (lsp-activate-on "protobuf")
      :server-id 'buf-lsp)))
  (add-hook 'protobuf-ts-mode-hook #'lsp-deferred))

(provide 'pimacs/lang-protobuf/config)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-protobuf/config.el")
;; End:
