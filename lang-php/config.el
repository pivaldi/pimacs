;;; pimacs/lang-php/config.el -*- lexical-binding: t; -*-
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

;; Auto-install tools if mise module is available
(when (modulep! :pimacs mise)
  (pim-mise-ensure-tools (file-name-directory load-file-name) t))

;; TODO : key bindings collision whit M-<tab> calls php-complete-function :
;; perform function completion on the text around point.

(defun pim--php-map (is-for-php-ts-mode-p)
  "Mapping for php-mode or php-ts-mode depending of IS-FOR-PHP-TS-MODE-P"
  (map!
   :when pim-azertyp
   :map (if is-for-php-ts-mode-p php-ts-mode-map php-mode-map)
   :desc "Insert \"->\". #pim" "²" (lambda nil
                                     (interactive)
                                     (insert "->"))
   :desc "Insert \"->\". #pim"    "œ" (lambda nil
                                        (interactive)
                                        (insert "->"))
   :desc "Fancy insert \"=>\". #pim" "¹" #'pim-php-insert-assoc-arrow
   :desc "Fancy insert \"=>\". #pim" "Œ" #'pim-php-insert-assoc-arrow
   )
  (map!
   :map (if is-for-php-ts-mode-p php-ts-mode-map php-mode-map)
   :desc "Compile Php file or lint it if prefixed. #pim" "C-c C-c" (lambda (&optional lint)
                                                                     (interactive "P")
                                                                     (pim-php-compile-file (buffer-file-name) lint))
   :desc "Copy the current namespace. #pim" "C-<f8>" #'pim-php-copy-namespace-from-buffer
   :desc "Copy class/method FQSEN from cursor context. #pim" "<C-S-f8>" #'pim-php-copy-fqsen
   :desc "Copy the current full qualified class name. #pim" "M-<f8>" #'pim-php-copy-class-from-buffer
   ))

(use-package! php-mode
  :defer t
  :init
  (let ((d (format "%s%s" doom-cache-dir "phpactor")))
    (unless (file-directory-p d)
      (make-directory d)))

  :config
  (setq php-warned-bad-indent t)
  (pim--php-map nil)

  :hook (php-mode-hook . php-enable-symfony2-coding-style)
  )

(when (modulep! +php-cs-fixer)
  (when (modulep! :lang php +lsp)
    (after! (:and php-mode lsp)
      (setq-hook! 'php-mode +format-with-lsp nil)
      )
    )

  (cl-defun pim--php-cs-fixer-apheleia (&key buffer formatter remote callback &allow-other-keys)
    "Called by `apheleia--run-formatter-function'.
:buffer buffer
     Original buffer being formatted. This shouldn't be
     modified. You can use it to check things like the
     current major mode, or the buffer filename. If you
     use it as input for the formatter, your formatter
     won't work when chained after another formatter.
:scratch scratch
     Buffer the formatter should modify. This starts out
     containing the original file contents, which will be
     the same as `buffer' except it has already been
     transformed by any formatters that ran previously.
     Name of the current formatter symbol, e.g. `black'.
:formatter
     The current formatter (a symbol)
:callback
     Callback. Should pass an error value (cons of symbol
     and data, like for `signal') or nil. For backwards
     compatibility it can also invoke only on success,
     with no args.
:remote remote
     The remote part of the buffers file-name or directory.
:async (not remote)
     Whether the formatter should be run async or not.
:callback
     Callback when formatting scratch has failed.
"
    (when (and (equal formatter 'php-cs-fixer) (not remote))
      (with-current-buffer buffer
        (php-cs-fixer-fix)
        (funcall callback))))

  (after! php-mode
    (use-package! php-cs-fixer
      :demand t
      :init
      (when (not (executable-find "php-cs-fixer"))
        (add-to-list 'pim-error-msgs "Please install php-cs-fixer : https://github.com/PHP-CS-Fixer/PHP-CS-Fixer"))
      :config
      (setq php-cs-fixer-fix-popup-on-error t)
      (setq-hook! 'php-mode-hook +format-with-lsp nil)
      (setq php-cs-fixer-config-option (format "%s/php-cs-fixer-config.php" (doom-module-locate-path '(:pimacs . lang-php))))
      ;; (add-hook! php-mode-hook (add-hook 'before-save-hook #'php-cs-fixer-before-save nil t))
      (with-eval-after-load 'apheleia
        (setf (alist-get 'php-cs-fixer apheleia-formatters)
              'pim--php-cs-fixer-apheleia)
        (setf (alist-get 'php-mode apheleia-mode-alist) '(php-cs-fixer))))))

(unless (or (modulep! +no-php-fh) (modulep! :pimacs treesit))
  (use-package! php-fh
    :defer t
    :autoload (php-fh-highlight)
    :init
    (after! php-mode (php-fh-highlight))))

(unless (modulep! :lang php +lsp)
  (after! (:and phpactor php-mode transient)
    (map!
     :map php-mode-map
     :desc "Execute Phpactor RPC goto_definition command. #pim" "M-." #'phpactor-goto-definition
     :desc "Execute Phpactor RPC references action to find references. #pim" "M-?" #'phpactor-find-references
     :desc "Open the Transient Php menu. #pim" "C-t" #'transient-php-menu)
    )

  (transient-define-prefix transient-php-menu ()
    "Phpactor Commands"
    [["Class"
      ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("cn"  "Namespace" phpactor-fix-namespace)]
     ["Properties"
      ("pa"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("pr" "Rename var locally" phpactor-rename-variable-local)
      ("pR" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract"
      ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods"
      ("mi" "Implement Contracts" phpactor-implement-contracts)
      ("mm"  "Generate method" phpactor-generate-method)]
     ["Navigate"
      ("nx" "List refs" phpactor-list-references)
      ("nX" "Replace refs" phpactor-replace-references)
      ("n."  "Goto def" phpactor-goto-definition)]
     ["Phpactor"
      ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)]]))

(unless (modulep! +no-dep)
  (load! "+dependencies"))

(when (modulep! :pimacs treesit)
  (load! "+treesit"))

(provide 'pimacs/lang-php)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/config.el")
;; End:
