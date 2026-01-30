;;; pimacs/treemacs/config.el -*- lexical-binding: t; -*-
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

(doom! :ui treemacs)

;; (setq +treemacs-git-mode 'simple)  ;; or 'none if you want zero git decoration

(use-package! treemacs
  :init
  ;; Keybinding
  (when pim-azertyp
    (map! :desc "Select/Unselect the treemacs window if it is visible. #pim"
          "M-à" #'treemacs-select-window))

  ;; Open at startup, but keep focus where it was
  (unless (modulep! +no-open)
    (defun pim/treemacs-open-no-focus ()
      (run-at-time
       0 nil
       (lambda ()
         (let ((win (selected-window)))
           (unless (eq (treemacs-current-visibility) 'visible)
             ;; Add current directory as project if workspace is empty (avoids prompt)
             (let ((ws (treemacs-current-workspace)))
               (when (or (null ws)
                         (null (treemacs-workspace->projects ws)))
                 (treemacs-do-add-project-to-workspace
                  default-directory
                  (file-name-nondirectory (directory-file-name default-directory)))))
             (treemacs))
           (when (window-live-p win)
             (select-window win))))))

    (add-hook! 'window-setup-hook #'pim/treemacs-open-no-focus))

  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)

  (when (modulep! :tools lsp)
    (lsp-treemacs-sync-mode 1)
    (setq lsp-treemacs-error-list-expand-depth 1000))

  ;; ;; Make Treemacs workspaces/perspectives aware
  ;; (when (modulep! :ui workspaces)
  ;;   ;; ensure treemacs-persp is available before setting scope
  ;;   (use-package! treemacs-persp
  ;;     :after (treemacs persp-mode)
  ;;     :config
  ;;     (treemacs-set-scope-type 'Frames)
  ;;     ))
  )


(provide 'pimacs/treemacs)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/treemacs/config.el")
;; End:
