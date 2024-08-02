;;; Package --- pi desktop and recentf configuration
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

(when (require 'recentf "recentf.elc" t)
  (recentf-mode 1))

;; If desktop saving is turned on, the state of Emacs is saved from
;; one session to another
;; The first time you save the state of the Emacs session, you must do
;; it manually, with the command `M-x desktop-save'.
;;;###package desktop
(use-package! desktop
 :config
  ;; The directory in which the desktop file should be saved.
  ;; Name of file for Emacs desktop, excluding the directory part.
  (setq desktop-base-file-name (concat ".desktop-" (user-real-login-name))
        desktop-save t
        ;; Does not save this files
        desktop-buffers-not-to-save
        (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.el\\.gz\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$")
        )
  (desktop-save-mode 1)
  ;; Does not save this modes
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

;; Save the place of point
(require 'saveplace)
(setq-default save-place t) ;; activation

;; Save action history.
(when (require 'savehist "savehist.elc" t) ;; Part of emacs22+
  (savehist-mode t))

;; Local variables:
;; coding: utf-8
;; End:
