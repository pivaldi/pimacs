;;; pimacs/neotree/autoload.el -*- lexical-binding: t; -*-
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

;;; Code:

;;;###autoload
(defun pim-neotree-sync-to-buffer ()
  "Open or synchronize NeoTree to the current filename."
  (interactive)
  (if (neo-global--window-exists-p)
      (save-excursion
        (neo-global--open-and-find (buffer-file-name))
        (with-selected-window (neo-global--get-window)
          (recenter-top-bottom)))
    (neotree-toggle)))

;;;###autoload
(defun pim-neotree-project-dir ()
  "Open NeoTree using the projectile root dir."
  (interactive)
  (when (not (neo-global--window-exists-p))
    (neotree-toggle)
    )
  (save-excursion
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neo-global--open-and-find project-dir)
            (with-selected-window (neo-global--get-window)
              (recenter-top-bottom)))
        (message "Could not find project root directory.")
        ))))

;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/neotree/autoload.el")
;; End:
