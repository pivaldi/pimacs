;;; pimacs/parens/config.el -*- lexical-binding: t; -*-
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

(when (modulep! :config default +smartparens)
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively.
Comes from http://ebzzry.com/en/emacs-pairs/"
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "pim-wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     ,(format "Wrap the following expression with %ss." (prin1-to-string key))
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren        . "(")
              (bracket      . "[")
              (brace        . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote   . "`")))

  (after! smartparens
    (show-smartparens-global-mode t)
    (smartparens-global-mode 1)
    (smartparens-strict-mode -1)
    ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
    ;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
    ;; (add-hook 'text-mode-hook 'turn-on-smartparens-mode)

    (defun pi-sp-delete-word-or-unwrap-sexp ()
      "If there is a word at point, delete it.
If there is a paired char at point use sp-unwrap-sexp."
      (interactive)
      (if (or
           (sp--looking-at (sp--get-opening-regexp (sp--get-pair-list-context 'navigate)))
           (sp--looking-at (sp--get-closing-regexp (sp--get-pair-list-context 'navigate)))
           )
          (sp-unwrap-sexp)
        (let* ((oldpoint (point)) (start (point)) (end (point))
               (syntaxes "w")
               (not-syntaxes (concat "^" syntaxes)))
          (skip-syntax-backward syntaxes) (setq start (point))
          (goto-char oldpoint)
          (skip-syntax-forward syntaxes) (setq end (point))
          (if (bolp)
              (progn
                (skip-syntax-forward not-syntaxes
                                     (save-excursion (end-of-line)
                                                     (point)))
                (setq start (point))
                (skip-syntax-forward syntaxes)
                (setq end (point)))
            (setq end (point))
            (skip-syntax-backward syntaxes)
            (setq start (point)))
          (unless (= start end)
            (delete-region start end)))))

    ;; Read this article : https://ebzzry.com/en/emacs-pairs/
    (map!
     :map smartparens-mode-map
     ;; All comments already are in the Doom module  :default +smartparens
     ;; :desc ". #pim" "C-M-a" sp-beginning-of-sexp
     ;; :desc ". #pim" "C-M-e" sp-end-of-sexp

     ;; :desc ". #pim" "C-M-f" sp-forward-sexp
     ;; :desc ". #pim" "C-M-b" sp-backward-sexp

     ;; :desc ". #pim" "C-M-n" sp-next-sexp
     ;; :desc ". #pim" "C-M-p" sp-previous-sexp

     :desc "Move point to the next position that is the end of a symbol. #pim" "C-S-f" #'sp-forward-symbol
     :desc "Move point to the next position that is the begining of a symbol. #pim" "C-S-b" #'sp-backward-symbol

     :desc ". #pim" "C-M-t" nil ;; Default is sp-transpose-sexp. Don't like this !
     ;; :desc ". #pim" "C-M-k" sp-kill-sexp
     :desc "Kill the balanced expression preceding point. #pim" "M-k" #'sp-backward-kill-sexp
     :desc "Jump to beginning of the sexp the point is in then kill the sexp respecting delimiters. #pim" "C-M-w" #'pim-sp-kill-sexp
     :desc "Rewrap the enclosing expression with a different pair. #pim" "C-S-M-r" #'sp-rewrap-sexp

     :desc "If word at point delete it and if paired char at point unwrap sexp. #pim" "C-<delete>" #'pim-sp-delete-word-or-unwrap-sexp
     ;; :desc ". #pim" "M-<delete>" #'sp-unwrap-sexp
     ;; :desc ". #pim" "M-<backspace>" #'sp-backward-unwrap-sexp
     :desc ". #pim" "C-<backspace>" #'sp-backward-delete-word
     ;; :desc ". #pim" [remap sp-backward-delete-word] backward-delete-word
     ;; :desc ". #pim" [remap sp-backward-kill-word] backward-kill-word

     :desc "Wrap the following expression with parens. #pim" "C-c ("  #'pim-wrap-with-parens
     :desc "Wrap the following expression with brackets. #pim" "C-c ["  #'pim-wrap-with-brackets
     :desc "Wrap the following expression with braces. #pim" "C-c {"  #'pim-wrap-with-braces
     :desc "Wrap the following expression with quotes. #pim" "C-c '"  #'pim-wrap-with-single-quotes
     :desc "Wrap the following expression with double quotes. #pim" "C-c \\" #'pim-wrap-with-double-quotes
     :desc "Wrap the following expression with underscores. #pim" "C-c _"  #'pim-wrap-with-underscores
     :desc "Wrap the following expression with back quotes. #pim" "C-c `"  #'pim-wrap-with-back-quotes
     )
    )
  )

(provide 'pimacs/pairing)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/pairing/config.el")
;; End:
