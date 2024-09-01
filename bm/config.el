;;; pimacs/bm/config.el -*- lexical-binding: t; -*-
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

;; Visible bookmarks in buffer : this package provides visible, buffer local,
;; bookmarks and the ability to jump forward and backward to the next bookmark.

;;; Code:

(use-package! bm
  :defer t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Disallow cross-buffer 'next'. Use native Emacs bookmark to navigate accross
  ;; buffer bookmarks !
  (setq bm-cycle-all-buffers nil)

  ;; where to store persistant files
  (setq bm-repository-file (concat doom-data-dir "bm-repository"))

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; The buffer should be recentered around the bookmark
  ;; after a `bm-next' or a `bm-previous'.
  (setq bm-recenter t)

  ;; Bookmars are highlighted in the line and in the right fringe.
  (setq bm-highlight-style 'bm-highlight-line-and-fringe)
  (setq bm-marker 'bm-marker-right)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hook   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  :preface
  (unless (modulep! +no-key-bindings)
    (map! :desc "Jump to next visible buffer local bookmark. #pim" "<f2>" #'bm-next)
    (map! :desc "Jump to previous visible buffer local bookmark. #pim" "S-<f2>" #'bm-previous)
    (map! :desc "Jump to previous visible buffer local bookmark. #pim" "C-<f2>" #'bm-toggle)
    (map! :desc "Toggle visible buffer local persistence. #pim" "s-<f2>" #'bm-toggle-buffer-persistence)))

(provide 'pimacs/bm)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/bm/config.el")
;; End:
