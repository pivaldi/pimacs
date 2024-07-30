;;; Package --- pi hook configuration for general purpoose
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


(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it ?" dir)))
                  (make-directory dir t))))))

;; Except for makefile-mode…
(after! make-mode
        (add-hook
         'makefile-mode-hook
         '(lambda()
            (setq indent-tabs-mode t)
            )))

(after! lisp-mode
        (add-hook 'lisp-mode-hook 'turn-off-auto-fill))

;; --------------------------------------------
;; * Make certain files executable when saved *
(defun pi-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable
From http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))
(add-hook 'after-save-hook 'pi-make-script-executable)

(provide 'pi-hook)
;;; pi-hook.el ends here

;; Local variables:
;; coding: utf-8
;; End:
