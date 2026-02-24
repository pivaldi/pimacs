;;; pimacs/visual-fill-column/config.el -*- lexical-binding: t; -*-
;; Copyright (c) 2026, Philippe Ivaldi <www.piprime.fr>

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

;; Visual Fill Column module - soft-wraps text at a specified column width
;; without modifying file contents.
;;
;; Flags:
;;   +writing-modes  Activate for org-mode, markdown-mode, text-mode
;;   +visual-line    Hook into visual-line-mode-hook globally
;;   +center         Enable visual-fill-column-center-text

;;; Code:

(defcustom pim-visual-fill-column-width nil
  "Column width for visual-fill-column-mode.
When nil, uses `fill-column'. Set to an integer for a fixed width."
  :type '(choice (const :tag "Use fill-column" nil)
          (integer :tag "Fixed width"))
  :group 'pimacs)

(use-package! visual-fill-column
  :defer t
  :init
  (setq visual-fill-column-width pim-visual-fill-column-width)
  (when (modulep! +center)
    (setq visual-fill-column-center-text t))
  :config
  ;; Fix window splitting with centered text (treemacs, etc.)
  (setq visual-fill-column-enable-sensible-window-split t))

;; visual-fill-column needs visual-line-mode for proper soft-wrapping
(add-hook! 'visual-fill-column-mode-hook (visual-line-mode 1))

;; Enable visual-wrap-prefix-mode for proper indentation of wrapped lines (Emacs 29+)
(add-hook 'visual-fill-column-mode-hook #'visual-wrap-prefix-mode)

;; +visual-line takes precedence over +writing-modes
(if (modulep! +visual-line)
    (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (when (modulep! +writing-modes)
    (add-hook 'org-mode-hook #'visual-fill-column-mode)
    (add-hook 'markdown-mode-hook #'visual-fill-column-mode)
    (add-hook 'text-mode-hook #'visual-fill-column-mode)))

(provide 'pimacs/visual-fill-column)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/visual-fill-column/config.el")
;; End:
