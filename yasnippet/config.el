;;; pimacs/yasnippet/config.el -*- lexical-binding: t; -*-
;;; Copyright (c) 2016, Philippe Ivaldi <www.piprime.fr>
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

;;; Code:

(after! yasnippet
  :config
  ;; `yas/next-field-key' can trigger stacked expansions.
  (setq yas-triggers-in-field t)

  (unless (modulep! :pimacs yasnippet +no-keys)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (map! :map yas-minor-mode-map
          :desc "" "<f3>" #'yas-expand)))

(provide 'pimacs/yasnippet)
;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/yasnippet/config.el")
;; End:
