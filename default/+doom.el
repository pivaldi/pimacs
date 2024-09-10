;;; -*- lexical-binding: t; -*-
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

;; Doom consider some buffers as unreal buffer (like *Message*)
;; I want to see this buffers !!
(setq doom-unreal-buffer-functions '(minibufferp))

(after! doom-modeline
  ;; Add Persp Workspace name in the Doom modeline.
  ;; https://github.com/doomemacs/doomemacs/issues/314#issuecomment-470381785
  (setq doom-modeline-persp-name t)
  ;; Completely disable management of the mode-line in popups
  ;; See https://github.com/doomemacs/doomemacs/blob/master/modules/ui/popup/README.org#disabling-hidden-mode-line-in-popups
  (when (modulep! :ui popup)
    (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/+doom.el")
;; End:
