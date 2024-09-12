;;; pimacs/lsp/config.el -*- lexical-binding: t; -*-
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

(unless (modulep! :tools lsp) (doom! :tools lsp))

;; See https://emacs.stackexchange.com/q/81247/45315
(when (executable-find "semgrep")
  (setq pim-warning-suppress-message-regexps '(".*semgrep/rulesRefreshed.*")))

(when  (and (not (modulep! :tools lsp +peek)) (modulep! +doc))
  ;; See this excellent documentation https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (after! lsp-ui
    (setq lsp-ui-doc-enable t)
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
     ))
  (use-package! lsp
    :hook (lsp-mode-hook . (lsp-signature-activate))))

(provide 'pimacs/lsp)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lsp/config.el")
;; End:
