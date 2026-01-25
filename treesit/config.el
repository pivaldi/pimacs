;;; Package pimacs/treesit --- PIMacs tree-sitter config
;;; pimacs/treesit/config.el -*- lexical-binding: t; -*-
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

(when (boundp 'pim-error-msgs)
  (when (< emacs-major-version 29)
    (add-to-list 'pim-error-msgs "Emacs 29+ is needed by treesit module"))

  (unless (fboundp 'treesit-install-language-grammar)
    (add-to-list
     'pim-error-msgs
     "Native treesit module *not* found.  Complie Emacs with configuration option --with-tree-sitter")))

;; Define all tree-sitter grammar sources
;; Using tags compatible with Emacs 30 tree-sitter ABI (version 14)
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.21.4"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.3"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1"))
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.21.0"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make" "v1.1.0"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.22.8" "php/src"))
        (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc" "master"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.6.1"))))

(defun pim-treesit-install-all-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist'.
Useful for pre-installing grammars in Docker or fresh installations."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar: %s" lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error
           (message "Failed to install %s: %s" lang (error-message-string err))))))))

(when (fboundp 'use-package!)
  (use-package! treesit
    :config
    (setq treesit-font-lock-level 4) ;; Maximum treesit font decoration
    ;; Add default tree-sitter directory to load path (for batch-installed grammars)
    (add-to-list 'treesit-extra-load-path
                 (expand-file-name "tree-sitter" user-emacs-directory))
    (pim-treesit-install-all-grammars)
    (dolist (mapping
             '((python-mode . python-ts-mode)
               (php-mode . php-ts-mode)
               (css-mode . css-ts-mode)
               (typescript-mode . typescript-ts-mode)
               (js2-mode . js-ts-mode)
               (bash-mode . bash-ts-mode)
               (conf-toml-mode . toml-ts-mode)
               (go-mode . go-ts-mode)
               (css-mode . css-ts-mode)
               (json-mode . json-ts-mode)
               (yaml-mode . yaml-ts-mode)
               (js-json-mode . json-ts-mode)))
      (add-to-list 'major-mode-remap-alist mapping))
    ))

;; (use-package! combobulate
;;   :custom
;;   (combobulate-key-prefix "C-c b")
;;   :hook (prog-mode . combobulate-mode)
;;   )

(provide 'pimacs/treesit)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/treesit/config.el")
;; End:
