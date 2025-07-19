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

;; Commentary:

;; THANKS:

;; BUGS:

;; INSTALLATION:

;; Code:

;; Except for makefile-mode…
(after! make-mode
  (add-hook
   'makefile-mode-hook
   (lambda()
     (setq indent-tabs-mode t)
     )))


(defvar pim-compilation-filenames '("Makefile" "makefile" "MakeFile" "makeFile"))

(defun pim-get-dominating-compilation-dir ()
  "Seeking a makefile recursively in directories higher"
  (let ((dir nil)
	(filenames pim-compilation-filenames)
        (filename nil))
    (while (and (not dir)
                (setq filename (pop filenames)))
      (setq dir (locate-dominating-file default-directory filename)))
    dir))


(defun pim-compile-above-makefile (&optional make-entry)
  "Calls `compile' emacs command on \"make MAKE-ENTRY\""
  (interactive "P")
  (let* ((mkf (pim-get-dominating-compilation-dir))
         (default-directory (directory-file-name mkf))
         (mke (if make-entry (format " %s" (read-string "Make entry: ")) "")))
    (if default-directory
        (progn
          (cd default-directory)
          (compile (concat "[ -e ./.envrc ] && source .envrc; make" mke))))))

(global-set-key (kbd "<f9>") 'pim-compile-above-makefile)


(provide 'pimacs/lang-make)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-make/config.el")
;; End:
