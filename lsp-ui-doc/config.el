;;; pimacs/lsp-ui-doc/config.el -*- lexical-binding: t; -*-
;;; Package pimacs/keys --- PIMacs key binding -*- lexical-binding: t; -*-
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

(unless (or (not (modulep! :tools lsp)) (modulep! :tools lsp +peek))
  ;; See this excellent documentation https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (use-package! lsp-ui
    :defer t
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-delay 0.33)
    (map!
     :map lsp-mode-map
     (:when pim-azertyp
       :desc "Toogle lsp-ui-doc-show-with-cursor. #pim" "C-!" #'pim-toggle-lsp-ui-doc-show-with-cursor)
     :desc "lsp-describe-thing-at-point -- lsp documentation of the thing at point. #pim" "C-§" #'lsp-describe-thing-at-point
     (:unless pim-azertyp
       :desc "lsp-describe-thing-at-point -- lsp documentation of the thing at point. #pim" "C-\"" #'lsp-describe-thing-at-point
       :desc "Toogle lsp-ui-doc-show-with-cursor. #pim" "C-," #'pim-toggle-lsp-ui-doc-show-with-cursor)
     )
    :hook (lsp-mode-hook . (lsp-ui-doc-mode lsp-signature-activate))
    ))

(provide 'pimacs/lsp-ui-doc)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lsp-ui-doc/config.el")
;; End:
