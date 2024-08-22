#!/usr/bin/env doomscript
;;; pimacs/default/cli.el --- See README.md -*- lexical-binding: t; -*-
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

:;; Commentary:

;;; Code:

;; (defun pim-defcli-generate-map-refcard (keymapname module)
;;   "Dynamically generate PIMacs command cli.
;; The generated cli is generate-KEYMAPNAME-refcard interpolating KEYMAPNAME."
;;   (let ((cli-name (intern (format "generate-%s-refcard" keymapname))))
;;     (defcli! (keys ,cli-name) ()
;;              "Export into a file the PIMacs key bindings added/modified to/in %s.
;; If keymap name is not added to the alist 'pim-keymapname-alist`
;; this cli does nothing." ;; TODO : make it dynamic to include the keymap name.
;;              ;; (:documentation (format "Export the PIMacs %s key bindings into a file." keymapname))
;;              (let ((byte-compile-current-file t))
;;                (require 'doom-start)
;;                (if (assoc keymapname pim-keymapname-alist)
;;                    (pim-keys-bindings-to-md-refcard keymapname module)
;;                  (print! (warn
;;                           "The keymap %s is not registered in pim-keymapname-alist by %s."
;;                           keymapname (symbol-name module)))
;;                  )
;;                )))
;;   )

;; (defcli! generate-global-map-refcard ()
;;          "Export the PIMacs global-map key bindings into the file key-bindings-refcard.md of the project."
;;          (let ((byte-compile-current-file t))
;;            (require 'doom-start)
;;            (pim-keys-bindings-to-md-refcard "global-map" 'keys)
;;            )
;;          )

;; Generate global-map keys refcard cli generation for modified keymap bindings.
;; (pim-defcli-generate-map-refcard "global-map" 'keys)

;; Generate at once all keys refcard cli generation for modified keymap bindings.
(defcli! (:root doom +pimacs) () "The Doom PIMacs CLI")
(defcli! (:root doom +pimacs keys) () "Keys related commands")
(defcli! (:root doom +pimacs keys generate-refcards) () ;; TODO : add option to generate all the binding in one file.
         "Export into the files (one by module) all the PIMacs key bindings added/modified."
         (let ((fname "")
               (keymapnames nil)
               (modulename nil)
               (module nil)
               (byte-compile-current-file t) ;; Needed by doom-start to enable the map! macro
               ;; (noninteractive nil)
               )
           (require 'doom-start)
           (require 'which-key)
           (general-override-mode)
           (dolist (km pim-keymapname-alist)
             (progn
               (setq
                modulename (car km)
                keymapnames (cdr km)
                module (intern modulename)
                fname (concat (doom-module-locate-path :pimacs module) "/key-bindings-refcard.md"))
               (print! (green (format "Generating %s key binding from module %s…" keymapnames modulename)))
               (pim/keys-bindings-to-md-refcard keymapnames fname)
               (print! (green (format "…wrote into %s" fname)))))))

(provide 'pimacs/default/cli)

;;; cli.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/cli.el")
;; End:
