;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2024, Philippe Ivaldi <www.piprim.net>
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

;;;; Commentary:

;; Configuration for typescript-ts-mode ()

;;; Code:

(after! typescript-mode
  (defvar pim-ng-app-filenames '("app.component.ts"))
  (defvar pim-ts-compile-command "deno")
  (when (modulep! +lsp)
    (add-hook 'typescript-mode-hook #'lsp))

  (use-package! ng2-mode
    :defer t
    :config

    (map! :map ng2-html-mode-map
          :desc "" "M-." #'ng2-html-goto-binding
          :map ng2-ts-mode-map
          :desc "" "<S-iso-lefttab>" #'pim-ng-complete-filename)

    (if (modulep! +lsp)
        (after! lsp
          (add-hook 'ng2-ts-mode #'lsp))
      (after! tide
        (add-hook 'typescript-mode-hook #'pim-setup-tide-mode)
        (add-hook 'typescript-ts-mode-hook #'pim-setup-tide-mode)
        ))

    (after! flycheck
      (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
      (unless (modulep! +lsp)
        (after! tide
          (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)))))

  (add-to-list 'auto-mode-alist '("\\.page.ts\\'" . ng2-ts-mode))

  (if (not (executable-find pim-ts-compile-command))
      (add-to-list 'pim-error-msgs "Please install deno : https://github.com/denoland/deno"))

  (add-hook 'typescript-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'pim-ts-compile)))
  )

(provide 'pimacs/lang-ts)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-ts/config.el")
;; End:
