;;; pimacs/lang-js/config.el -*- lexical-binding: t; -*-
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

;; Configuration for JavaScript mode (js-mode / js-ts-mode)

;;; Code:

(defun pim--js-map (is-for-js-ts-mode-p)
  "Setup keybindings for js-mode or js-ts-mode depending on IS-FOR-JS-TS-MODE-P."
  (map!
   :map (if is-for-js-ts-mode-p js-ts-mode-map js-mode-map)
   :desc "Compile JavaScript file. #pim" "C-c C-c" #'pim-js-compile-file))

(defun pim-js-compile-file (filename)
  "Compile FILENAME with node."
  (interactive (list (buffer-file-name)))
  (compile (format "node %s" (shell-quote-argument filename))))

(use-package! js
  :defer t
  :config
  (setq js-indent-level 2)
  (pim--js-map nil))

(when (modulep! :pimacs treesit)
  (load! "+treesit"))

(provide 'pimacs/lang-js)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-js/config.el")
;; End:
