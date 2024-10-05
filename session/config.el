;;; Package pimacs/session --- description here  -*- lexical-binding: t; -*-
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

(unless (modulep! :ui workspaces)
  (doom! :ui workspaces))

;; Restoring workspace
(add-hook! 'window-setup-hook
  (progn (doom/quickload-session t)))

;; (add-hook! 'window-setup-hook
;;   (progn
;;     ()
;;     (doom-load-session file)))


(use-package! desktop
  :config
  (setq desktop-buffers-not-to-save
        (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.el\\.gz\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))
  ;; Do not reopen the following modes :
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (setq desktop-save nil)
  (setq desktop-auto-save-timeout nil)
  ;; (setq desktop-auto-save-timeout 30)
  ;; (desktop-save-mode 1) ;; I use :pimacs session
  )

(defvar pim-doom-session-auto-save-keeped-backup-num
  (if (boundp 'pim-doom-session-auto-save-keeped-backup-num) pim-doom-session-auto-save-keeped-backup-num 5)
  "Number of autosaved Doom session keeped.")

(defvar pim-doom-session-auto-save-timeout
  (if (boundp 'pim-doom-session-auto-save-timeout) pim-doom-session-auto-save-timeout 60)
  "Number of seconds of idle time before auto saving the Doom session.")

(defvar pim-current-persp-auto-save-num pim-doom-session-auto-save-keeped-backup-num)
(defvar pim-doom-session-auto-save-timer nil)
(defvar pim-doom-session-auto-save-lock nil)
(defvar pim-doom-base-lock-name ".pim-doom-session.lock")
(defvar pim-auto-save-fname "pim-autosave-")

(unless (modulep! +no-auto-save)
  (unless noninteractive
    (add-hook 'kill-emacs-query-functions #'pim--doom-session-on-kill)
    ;; Certain things should be done even if
    ;; `kill-emacs-query-functions' are not called.
    (add-hook 'kill-emacs-hook #'pim--doom-session-on-kill)
    (add-hook! 'window-setup-hook #'pim-doom-session-auto-save-enable))
  )

(after! persp-mode
  (setq persp-auto-save-fname "on-shutdown")
  (setq persp-auto-save-num-of-backups 5)
  (setq persp-auto-resume-time 1)
  )

(provide 'pimacs/session)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/session/config.el")
;; End:
