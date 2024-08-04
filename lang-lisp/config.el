;;; pimacs/pim-lang-lisp/config.el -*- lexical-binding: t; -*-
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

;; Code:

(after!
 lisp-mode
 (add-hook 'lisp-mode-hook 'turn-off-auto-fill))

;; ;; TODO : add this face to others programming langages !!
(after!
 pimacs/theme
 (add-hook 'emacs-lisp-mode-hook
           (lambda ()
             (font-lock-add-keywords
              'nil
              '(("\\(;; \\*=*\\*$\\)" 1 'pim-comment-section-face t)
                ("\\(;; \\*\\..*\\.\\*$\\)" 1 'pim-comment-section-face t)
                ("\\(;; -*\n;; \\*.*\\*$\\)" 1 'pim-comment-sub-section-face t)))))
 (font-lock-add-keywords
  'emacs-lisp-mode-hook
  '(("\\(;; -*\n;; \\*.*\\*$\\)" 1 'pim-comment-sub-section-face t)))
 )

(provide 'pimacs/lang-lisp)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-lisp/config.el")
;; End:
