;;; pimacs/neotree/config.el -*- lexical-binding: t; -*-
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

(doom! :ui neotree)

(map! :desc "Toggle show the NeoTree window. #pim" "<C-f7>" #'neotree-toggle)

(use-package! neotree
  :defer t
  :init
  (setq neo-window-fixed-size nil
        neo-smart-open t ;; Every time when the neotree window is opened, let it find current file and jump to node.
        )
  (after! projectile-mode
    ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
    ;; Remove the unwanted +workspaces-switch-to-project-h
    (setq projectile-switch-project-action #'neotree-projectile-action)
    (map!
     :desc "Open NeoTree using the projectile root dir. #pim" "<S-f7>" #'pim-neotree-sync-to-buffer
     :desc "Open or synchronize NeoTree to the current filename. #pim" "<f7>" #'pim-neotree-project-dir))
  )


(provide 'pimacs/neotree)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/neotree/config.el")
;; End:
