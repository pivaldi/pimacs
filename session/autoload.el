;;; pimacs/session/autoload.el -*- lexical-binding: t; -*-
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

;; Most codes are modified versions of native Emacs desktop-save-mode codes.

;;; TODO
;; Make it a mode like desktop-auto-save-mode.

;;; Code:

(defun pim--doom-session-full-lock-name ()
  "Return the full name of the desktop lock file."
  (doom-session-file pim-doom-base-lock-name))

(defun pim--doom-session-claim-lock ()
  "Record this Emacs process as the owner of the pim session file."
  (let ((file (pim--doom-session-full-lock-name)))
    (unless (file-exists-p file)
      (write-region (number-to-string (emacs-pid)) nil
                    file))))

(defun pim--doom-session-release-lock()
  "Remove the lock file for the pim session."
  (let ((file (pim--doom-session-full-fname)))
    (when (file-exists-p file) (delete-file file))))

(defun pim--doom-session-full-fname ()
  "Return the full name of the pim session file."
  (let ((persp-auto-save-fname pim-auto-save-fname))
    (format "%s%s" (doom-session-file) pim-current-persp-auto-save-num)))

(defun pim-doom-session-auto-save ()
  "Save the Doom session periodically.
Called by the timer created in `pim--doom-session-auto-save-set-timer'."
  (when (and
         (integerp pim-doom-session-auto-save-timeout)
         (> pim-doom-session-auto-save-timeout 0)
         ;; Avoid desktop saving during lazy loading.
         (not desktop-lazy-timer)
         ;; Save only to own desktop file.
         (eq (emacs-pid) (pim--doom-session-owner))
         desktop-dirname)
    (doom-save-session (pim--doom-session-full-fname))
    (setq pim-current-persp-auto-save-num (- pim-current-persp-auto-save-num 1))
    (when (< pim-current-persp-auto-save-num 1)
      (setq pim-current-persp-auto-save-num persp-auto-save-num-of-backups)
      )))

(defun pim--doom-session-owner ()
  "Return the PID of the Emacs process that owns the pim session.
Return nil if no pim session lock file found or no Emacs process is using it."
  (let (owner
        (file (pim--doom-session-full-lock-name)))
    (and (file-exists-p file)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents-literally file)
             (goto-char (point-min))
             (setq owner (read (current-buffer)))
             (integerp owner)))
         owner)))

(defun pim--doom-session-auto-save-set-timer ()
  "Set the Doom session auto-save timer.
Cancel any previous timer. When `pim-doom-session-auto-save-timeout' is
a positive integer, start a new idle timer to call `pim-doom-session-auto-save'
after that many seconds of idle time. This function is called from
`window-configuration-change-hook'."
  (pim-doom-session-auto-save-cancel-timer)
  (when (and (integerp pim-doom-session-auto-save-timeout)
             (> pim-doom-session-auto-save-timeout 0))
    (setq pim-doom-session-auto-save-timer
          (run-with-idle-timer
           pim-doom-session-auto-save-timeout nil
           'pim-doom-session-auto-save))))

;;;###autoload
(defun pim-doom-session-auto-save-enable ()
  "Enable the pim auto save feature."
  (interactive)
  (when (and (integerp pim-doom-session-auto-save-timeout)
             (> pim-doom-session-auto-save-timeout 0))
    (pim--doom-session-claim-lock)
    (add-hook 'window-configuration-change-hook #'pim--doom-session-auto-save-set-timer)))

;;;###autosave
(defun pim-doom-session-auto-save-disable ()
  "Disable the pim auto save feature."
  (interactive)
  (remove-hook 'window-configuration-change-hook #'pim--doom-session-auto-save-set-timer)
  (pim--doom-session-auto-save-cancel-timer))

(defun pim--doom-session-auto-save-cancel-timer ()
  (when pim-doom-session-auto-save-timer
    (cancel-timer pim-doom-session-auto-save-timer)
    (setq pim-doom-session-auto-save-timer nil)))

;;;###autoload
(defun pim--doom-session-on-kill ()
  ;; If we own it, we don't anymore.
  (when (eq (emacs-pid) (pim--doom-session-owner))
    ;; Allow exiting Emacs even if we can't delete the desktop file.
    (ignore-error file-error
      (pim--doom-session-release-lock))))

(provide 'pimacs/session/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/session/autoload.el")
;; End:
