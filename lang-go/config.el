;;; pimacs/lang-go/config.el -*- lexical-binding: t; -*-
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

(after! go-mode
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'pim-go_ts_mode-hook)
  (map!
   :map go-mode-map
   :desc "Jump to the definition of the expression at POINT. #pim" "M-." #'godef-jump)
  )

(after! go-ts-mode
  (map!
   :map go-ts-mode-map
   :desc "Jump to the definition of the expression at POINT. #pim" "M-." #'godef-jump)

  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-ts-mode-hook #'pim-go_ts_mode-hook)
  )



(provide 'pimacs/lang-go/config)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-go/config.el")
;; End:
