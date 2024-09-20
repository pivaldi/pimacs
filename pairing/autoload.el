;;; pimacs/pairing/autoload.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun pim-sp-kill-sexp (arg)
  "Jump to beginning of the sexp then kill the sexp respecting delimiters."
  (interactive "*P")
  (sp-beginning-of-sexp arg)
  (sp-kill-hybrid-sexp nil))

;;;###autoload
(defun pim-sp-delete-word-or-unwrap-sexp ()
  "Word at point => delete it, paired char at point => `sp-unwrap-sexp'."
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


(provide 'pimacs/pairing/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/pairing/autoload.el")
;; End:
