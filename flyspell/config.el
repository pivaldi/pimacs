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
              ;; (or (string= pim-flyspell-prog-mode-dictionary pim-flyspell-default-dictionary)
              ;;     (ispell-change-dictionary pim-flyspell-prog-mode-dictionary))
              (flyspell-mode -1)
              ))

  (when (modulep! :checkers spell +everywhere)
    (add-hook! '(text-mode-hook)
               #'flyspell-mode))

  ;; Don't spellcheck org blocks
  ;; (pushnew! ispell-skip-region-alist
  ;;           '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
  ;;           '("#\\+BEGIN_SRC" . "#\\+END_SRC")
  ;;           '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (map!
   :desc "Toggle dictionary between two dictionaries. #pim" "<f6>" #'pim-ispell-dictionary-switch
   :desc "Toggle flyspell-mode . #pim" "S-<f6>" #'flyspell-mode)
  (map! :ie :desc "flyspell correct word or region. #pim" "M-$" #'pim-flyspell-correct)
  (map! :map flyspell-mouse-map "<mouse-1>" nil)
  (map! :map flyspell-mode-map "C-m" nil)
  ;; Remove fucking flyspell-correct-at-point on return key pressed.
  ;; When I press RET, I want insert a break-line.
  (map! :map flyspell-mouse-map
        "RET"    nil
        [return] nil
        ;; [mouse-1] #'flyspell-correct-at-point
        )
  )


(provide 'pimacs/flyspell)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/flyspell/config.el")
;; End:
