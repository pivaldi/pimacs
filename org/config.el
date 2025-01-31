;;; Package pimacs/org-mode --- See README.md -*- lexical-binding: t; -*-
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

;; Make shift-cursor commands select text when possible in org-mode.
;; This option is only relevant at load-time of Org mode, and must be set
;; *before* org.el is loaded.
(when shift-select-mode
  (setq org-support-shift-select t
        org-replace-disputed-keys t)
  )

(after! org
  (setq org-link-descriptive t
        org-export-with-creator t
        org-export-with-author nil)

  (load! "+template"))

;; (add-hook 'org-mode-hook 'turn-off-auto-fill)

(after! ox
  ;; (setq org-export-creator-string "[[https://www.gnu.org/software/emacs/][Emacs]] \\& [[https://orgmode.org/][Org] mode")
  (setq org-export-creator-string "Emacs + Org Mode"))

(after! ox-html
  (add-to-list
   'org-html-postamble-format
   '(("fr" "<p class=\"author\">Auteur: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>")))
  (setq org-export-default-language (if (modulep! +lang-fr) "fr" "en")
        org-html-creator-string "With: <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> & <a href=\"https://orgmode.org\">Org</a> mode"
        )
  )

(provide 'pimacs/org-mode)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/org/config.el")
;; End:
