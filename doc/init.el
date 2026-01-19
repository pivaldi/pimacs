;;; pimacs/doc/init.el -*- lexical-binding: t; -*-
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

(defvar pim-doc-key-binding-prefixes-to-export (list)
  "List of key binding prefixes to be exported by `pim-generate-all-fundamental-key-bindings'.")

(setq pim-doc-key-binding-prefixes-to-export
      (list "" "C-c" "C-x" "C-h" "M-g" "M-s" "H-i"))

(defvar pim-doc-keymaps-to-export (list)
  "List of keymaps to be exported by `pim-generate-all-keymaps'.")
(setq pim-doc-keymaps-to-export
      (list '("org-mode-map" . org)
            '("dired-mode-map" . dired)
            '("isearch-mode-map" . isearch)
            '("magit-mode-map" . magit)
            '("projectile-mode-map" . projectile)
            '("ctl-x-map" . nil)
            '("doom-leader-workspaces/windows-map" . nil)
            '("minibuffer-local-map" . nil)
            '("emacs-lisp-mode-map" . elisp-mode)
            '("smartparens-mode-map" . smartparens)
            '("vertico-map" . vertico)
            ))

(when (modulep! :completion corfu)
  (add-to-list 'pim-doc-keymaps-to-export '("corfu-map" . corfu))
  )

(when (modulep! :pimacs lsp)
  (add-to-list 'pim-doc-keymaps-to-export '("lsp-mode-map" . lsp)))

(when (or (modulep! :pimacs lang-php) (modulep! :lang php))
  (add-to-list 'pim-doc-keymaps-to-export '("php-mode-map" . php-mode)))

(when (or (modulep! :pimacs lang-go) (modulep! :lang go))
  (add-to-list 'pim-doc-keymaps-to-export '("go-mode-map" . go-mode))
  (when (fboundp 'go-ts-mode)
    (add-to-list 'pim-doc-keymaps-to-export '("go-ts-mode-map" . go-ts-mode))))

(when (or (modulep! :pimacs lang-js) (modulep! :lang javascript))
  (add-to-list 'pim-doc-keymaps-to-export '("js-mode-map" . js))
  (when (fboundp 'js-ts-mode)
    (add-to-list 'pim-doc-keymaps-to-export '("js-ts-mode-map" . js))))

(provide `pimacs/doc/init)
;;; init.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/doc/init.el")
;; End:
