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
   :desc "Jump to the visible CHAR. #pim" "1" #'avy-goto-char
   :desc "Jump to the visible CHAR1 followed by CHAR2. #pim" "2" #'avy-goto-char-2
   :desc "Read one or many consecutive chars. #pim" "t" #'avy-goto-char-timer
   :desc "Jump above to the visible CHAR1 followed by CHAR2. #pim" "a" #'avy-goto-char-2-above
   :desc "Jump below to the visible CHAR1 followed by CHAR2. #pim" "b" #'avy-goto-char-2-below
   :desc "Jump to the visible CHAR in the current line. #pim" "l" #'avy-goto-char-in-line
   )
  (:prefix ("l" . "Avy goto line. #pim")
   :desc "Jump to a line start in current buffer. #pim" "l" #'avy-goto-line
   :desc "Jump above to a line start in current buffer. #pim" "a" #'avy-goto-line-above
   :desc "Jump below to a line start in current buffer. #pim" "b" #'avy-goto-line-below
   )
  (:prefix ("w" . "Avy goto word and sub-word. #pim")
   :desc "avy-goto-word-0 : jump to a word start. #pim" "0" #'avy-goto-word-0
   :desc "Jump above to a word start. #pim" "a" #'avy-goto-word-0-above
   :desc "jump below to a word start. #pim" "b" #'avy-goto-word-0-below
   :desc "Jump to the visible CHAR at a word start. #pim" "l" #'avy-goto-word-1
   :desc "Jump above to the visible CHAR at a word start. #pim" "A" #'avy-goto-word-1-above
   :desc "Jump below to the visible CHAR at a word start. #pim" "B" #'avy-goto-word-1-below
   )
  (:prefix ("s" . "Avy goto sub-word. #pim")
   :desc "Jump to a word or subword start. #pim" "s" #'avy-goto-subword-0
   :desc "Jump to the visible CHAR at a subword start. #pim" "S" #'avy-goto-subword-1
   :desc "Forward to avy-goto-subword-1 or avy-goto-word-1. #pim" "B" #'avy-goto-word-or-subword-1)))

(map!
 :after isearch
 :map isearch-mode-map
 :desc "Jump to one of the current isearch candidates. #pim" "C-a" #'avy-isearch)

(map!
 :prefix ("C-S-w" . "Avy kill functions. #pim")
 :desc "Avy kill region. #pim"     "r"  #'avy-kill-region
 :desc "Avy kill whole line. #pim" "l"  #'avy-kill-whole-line)

(map! ;; TODO : does not work :(
 :prefix ("M-W" . "Avy kill save functions. #pim")
 :desc "Avy kill-ring save region. #pim"     "r" #'avy-kill-ring-save-region
 :desc "Avy kill-ring save whole line. #pim" "l" #'avy-kill-ring-save-whole-line)

(provide 'pimacs/avy/+emacs)
;; +emacs.el ends here
;;
;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/avy/+emacs.el")
;; End:
