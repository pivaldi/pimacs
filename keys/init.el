;;; pimacs/keys/init.el --- See README.md -*- lexical-binding: t; -*-
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

;;; Code:

(add-to-list 'pim-keymapname-alist
             '("keys" .
               (("global-map" . nil)
                ("general-override-mode-map" . general)
                ("which-key-C-h-map" . which-key)
                ("minibuffer-local-map" . nil)
                ("prog-mode-map" . prog-mode))))

;; (setq pim-keymapname-alist
;;       (list '("keys" . (("global-map" . nil) ("general-override-mode-map" . general)))
;;             '("default" . (("go-mode-map" . go-mode)))))

(provide 'pimacs/keys/init)
;;; init.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/keys/init.el")
;; End:
