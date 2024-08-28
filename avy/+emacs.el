;;; pimacs/avy/+emacs.el -*- lexical-binding: t; -*-
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


(map!
 :prefix "M-g"
 (:prefix
  ("a" . "Avy goto. #pim")
  (:prefix ("c" . "Avy goto char. #pim")
   :desc "avy-goto-char : jump to the visible CHAR. #pim" "1" #'avy-goto-char
   :desc "avy-goto-char-2 : jump to the visible CHAR1 followed by CHAR2. #pim" "2" #'avy-goto-char-2
   :desc "avy-goto-char-timer : read one or many consecutive chars. #pim" "t" #'avy-goto-char-timer
   :desc "avy-goto-char-char-2-above : scoped version of avy-goto-char-2. #pim" "a" #'avy-goto-char-2-above
   :desc "avy-goto-char-2-below scoped version of avy-goto-char-2. #pim" "b" #'avy-goto-char-2-below
   :desc "avy-goto-char-in-line : jump to the visible CHAR in the current line. #pim" "l" #'avy-goto-char-in-line
   )
  (:prefix ("l" . "Avy goto line. #pim")
   :desc "avy-goto-line : jump to a line start in current buffer. #pim" "l" #'avy-goto-line
   :desc "avy-goto-line-above : scoped version of avy-goto-line. #pim" "a" #'avy-goto-line-above
   :desc "avy-goto-line-below : scoped version of avy-goto-line. #pim" "b" #'avy-goto-line-below
   )
  (:prefix ("w" . "Avy goto word and sub-word. #pim")
   :desc "avy-goto-word-0 : jump to a word start. #pim" "0" #'avy-goto-word-0
   :desc "avy-goto-word-0-above : scoped version. #pim" "a" #'avy-goto-word-0-above
   :desc "avy-goto-word-0-below : scoped version. #pim" "b" #'avy-goto-word-0-below
   :desc "avy-goto-word-1 : jump to the visible CHAR at a word start. #pim" "l" #'avy-goto-word-1
   :desc "avy-goto-word-1-above : scoped version. #pim" "A" #'avy-goto-word-1-above
   :desc "avy-goto-word-1-below : scoped version. #pim" "B" #'avy-goto-word-1-below
   )
  (:prefix ("s" . "Avy goto sub-word. #pim")
   :desc "avy-goto-subword-0 : jump to a word or subword start. #pim" "s" #'avy-goto-subword-0
   :desc "avy-goto-subword-1 : jump to the visible CHAR at a subword start. #pim" "S" #'avy-goto-subword-1
   :desc "avy-goto-word-or-subword-1 : forward to avy-goto-subword-1 or avy-goto-word-1. #pim" "B" #'avy-goto-word-or-subword-1)))

(map!
 :after isearch
 :map isearch-mode-map
 :desc "avy-isearch : jump to one of the current isearch candidates. #pim" "C-a" #'avy-isearch)

(provide 'pimacs/avy/+emacs)
;; +emacs.el ends here
;;
;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/avy/+emacs.el")
;; End:
