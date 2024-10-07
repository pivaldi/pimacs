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

(defvar pim-doom-session-auto-save-keeped-backup-num
  (if (boundp 'pim-doom-session-auto-save-keeped-backup-num) pim-doom-session-auto-save-keeped-backup-num 5)
  "Number of autosaved Doom session keeped.")

(defvar pim-doom-session-auto-save-timeout
  (if (boundp 'pim-doom-session-auto-save-timeout) pim-doom-session-auto-save-timeout 600)
  "Number of seconds of idle time before auto saving the Doom session.")

(defvar pim-doom-session-auto-save-timer nil)
(defvar pim-doom-session-auto-save-lock nil)
(defvar pim-doom-base-lock-name ".pim-doom-session.lock")
(defvar pim-auto-save-fname "pim-autosave-")
(defvar pim-persp-auto-save-fname "on-shutdown")

(defvar pim-current-auto-save-num nil
  "Internal use. Use the function `pim-current-auto-save-num'to set and
retrieve his value.")

;; Restoring workspace
(add-hook 'doom-init-ui-hook
          (lambda ()
            (require 'persp-mode)
            (doom-load-session
             (pim-latest-file
              (file-name-directory (doom-session-file))
              'full
              (rx (or (group (eval pim-auto-save-fname) digit)
                      (group (eval pim-persp-auto-save-fname) (? digit))))))))

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

(unless (modulep! +no-auto-save)
  (unless noninteractive
    (add-hook 'kill-emacs-query-functions #'pim--doom-session-on-kill)
    ;; Certain things should be done even if
    ;; `kill-emacs-query-functions' are not called.
    (add-hook 'kill-emacs-hook #'pim--doom-session-on-kill)
    (add-hook! 'doom-init-ui-hook #'pim-doom-session-auto-save-enable))
  )

(after! persp-mode
  (setq persp-auto-save-fname pim-persp-auto-save-fname)
  (setq persp-auto-save-num-of-backups 5)
  (setq persp-auto-resume-time 1)
  )

(provide 'pimacs/session)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/session/config.el")
;; End:
