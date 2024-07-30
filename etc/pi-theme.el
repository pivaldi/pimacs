;;; Package --- pi-theme configuration
;; Copyright (c) 2024, Philippe Ivaldi <www.piprime.fr>
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation ; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; CODE:

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(if
    (find-font (font-spec :name "TerminessTTF NF"))
    (setq
     default-font "TerminessTTF NF"
     default-font-size 14.0
     default-nice-size 12.0
     doom-font-increment 1
     doom-font (font-spec :family default-font :foundry "PfEd" :weight 'bold :size default-font-size)
     ;; This determines the style of line numbers in effect. If set to `nil', line
     ;; numbers are disabled. For relative line numbers, set this to `relative'.
     display-line-numbers-type nil)

  (add-to-list 'pi-error-msgs "Please install Terminess TTF Nerd Fond : https://github.com/ryanoasis/nerd-fonts"))

;; See useless white-spaces
(set-face-attribute 'trailing-whitespace nil
                    :background "#2F5555")
(setq whitespace-style '(face tabs trailing))

;; ------------------
;; * Comment header *
;; ;; TODO : add this face to others programming langages !!
(defface pi-comment-section-face
  `((t
     ( :foreground "yellow")))
  "Face used to highlighting header of comment section."
  :group 'pi-comment)
(defface pi-comment-sub-section-face
  `((t
     ( :foreground "white")))
  "Face used to highlighting header of comment section."
  :group 'pi-comment)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             'nil
             '(("\\(;; \\*=*\\*$\\)" 1 'pi-comment-section-face t)
               ("\\(;; \\*\\..*\\.\\*$\\)" 1 'pi-comment-section-face t)
               ("\\(;; -*\n;; \\*.*\\*$\\)" 1 'pi-comment-sub-section-face t)))))
(font-lock-add-keywords
 'emacs-lisp-mode-hook
 '(("\\(;; -*\n;; \\*.*\\*$\\)" 1 'pi-comment-sub-section-face t)))


(provide 'pi-theme)
;;; pi-theme.el ends here

;; Local variables:
;; coding: utf-8
;; End:
