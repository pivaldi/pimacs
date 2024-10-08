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
(add-hook! 'doom-after-init-hook :append
  (defun pim-org-support-shift-select-setup ()
    (when shift-select-mode
      (setq-default org-support-shift-select t)
      ))
  )

(after! org (setq org-link-descriptive nil))

(provide 'pimacs/org-mode)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/org/config.el")
;; End:
