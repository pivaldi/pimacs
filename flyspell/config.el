;;; pimacs/flyspell/confige.el -*- lexical-binding: t; -*-

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

(defvar pim-flyspell-default-dictionary "french")
(defvar pim-flyspell-secondary-dictionary "american")
(defvar pim-flyspell-prog-mode-dictionary "american")

(after! flyspell
  ;; I don't want to use M-TAB to correct, M-$ is enough for me.
  (setq flyspell-use-meta-tab nil)
  ;; (ispell-change-dictionary pim-flyspell-default-dictionary 1)
  (setq flyspell-default-dictionary pim-flyspell-default-dictionary)

  (add-hook 'prog-mode-hook
            (lambda ()
              (or (string= pim-flyspell-prog-mode-dictionary pim-flyspell-default-dictionary)
                  (ispell-change-dictionary pim-flyspell-prog-mode-dictionary))
              ))

  (when (modulep! :checkers spell +everywhere)
    (add-hook! '(text-mode-hook)
               #'flyspell-mode))

  (map!
   :desc "Toggle dictionary between two dictionaries. #pim" "<f6>" #'pim-ispell-dictionary-switch
   :desc "Toggle flyspell-mode . #pim" "S-<f6>" #'flyspell-mode)
  )

(provide 'pimacs/flyspell)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/flyspell/config.el")
;; End:
