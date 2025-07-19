;;; pimacs/default/hook.el -*- lexical-binding: t; -*-
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

(add-hook! 'doom-after-init-hook :append
  (defun pim--doom-after-init-hook-fn ()
    (global-visual-line-mode -1)
    (auto-fill-mode -1)
    (setq inhibit-auto-fill t) ;; TODO: Disabled completly auto-fill-mode, it's too hard !!
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (remove-hook 'prog-mode-hook #'turn-on-auto-fill)
    (remove-hook 'prog-mode-hook #'visual-line-mode)
    (after! yaml-mode
      (remove-hook 'yaml-mode-hook #'turn-on-auto-fill))
    (after! org-mode
      (remove-hook 'org-mode-hook #'turn-on-auto-fill))
    ))

;; ----------------------------------------------
;; * Auto-fill: automatic cutting of long lines *
;; So that the auto-fill mode does not cut off at the place of a ":" or ";" etc..
;; Author: Matieux Moy http://matthieu-moy.fr/spip/?lang=en
(defun pim-fill-nobreak-predicate ()
  (save-match-data
    (or (looking-at "[ \t]*[)}»!?;:]")
        (looking-at "[ \t]*\\.\\.\\.")
        (save-excursion
          (skip-chars-backward " \t")
          (backward-char 1)
          (looking-at "[([{«]")))))
(setq fill-nobreak-predicate (cons #'pim-fill-nobreak-predicate fill-nobreak-predicate))

(after!
  pimacs/default
  (dolist (hook pim-auto-fill-mode-hooks)
    (add-hook hook
              (lambda ()
                (auto-fill-mode 1))))
  )

(defun pim-stop-using-minibuffer ()
  "Abort the minibuffer when using the mouse.
See https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook #'pim-stop-using-minibuffer)

(add-hook 'after-save-hook #'pim-make-script-executable)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/+hooks.el")
;; End:
