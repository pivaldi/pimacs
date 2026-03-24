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

(defvar mise-modulep (modulep! :pimacs mise))

(when mise-modulep
  (load (doom-module-expand-path '(:pimacs . mise) "autoload.el")
        nil 'nomessage)
  )

(defun pim--go-tools-install-globally nil
  (if (modulep! :pimacs lang-go +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-go) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing Go Tools: %s" (pim-go-tools-install-globally)))))
    (print! (item "Go Tools not installed because pimacs/lang-go is not loaded with +tools option…" ))))

(defun pim--ts-tools-install-globally nil
  (if (modulep! :pimacs lang-ts +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-ts) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing Typescript Tools: %s" (pim-ts-tools-install-globally)))))
    (print! (item "Typescript Tools not installed because pimacs/lang-ts is not loaded with +tools option…" ))))

(defun pim--js-tools-install-globally nil
  (if (modulep! :pimacs lang-js +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-js) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing JavaScript Tools: %s" (pim-js-tools-install-globally)))))
    (print! (item "JavaScript Tools not installed because pimacs/lang-js is not loaded with +tools option…" ))))

(defun pim--php-tools-install-globally nil
  (if (modulep! :pimacs lang-php +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-php) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing PHP Tools: %s" (pim-php-tools-install-globally)))))
    (print! (item "PHP Tools not installed because pimacs/lang-php is not loaded with +tools option…" ))))

(defun pim--protobuf-tools-install-globally nil
  (if (modulep! :pimacs lang-protobuf +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-protobuf) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing Protobuf Tools: %s" (pim-protobuf-tools-install-globally)))))
    (print! (item "Protobuf Tools not installed because pimacs/lang-protobuf is not loaded with +tools option…" ))))

(defun pim--typst-tools-install-globally nil
  (if (modulep! :pimacs lang-typst +tools)
      (progn
        (load (doom-module-expand-path '(:pimacs . lang-typst) "autoload.el")
              nil 'nomessage)
        (print! (item (format "Installing Typst Tools: %s" (pim-typst-tools-install-globally)))))
    (print! (item "Typst Tools not installed because pimacs/lang-typst is not loaded with +tools option…" ))))

(add-hook!
 'doom-after-sync-hook
 (progn
   (print! (start "PIMacs Doom after sync hook"))
   (print-group!
     (when mise-modulep
       (print! (start "pimacs/mise is loaded"))
       (print-group!
         (pim--go-tools-install-globally)
         (pim--ts-tools-install-globally)
         (pim--js-tools-install-globally)
         (pim--php-tools-install-globally)
         (pim--protobuf-tools-install-globally)
         (pim--typst-tools-install-globally)
         )))))

(defcli!
 (:root doom +pimacs) () "The Doom PIMacs CLI"
 (message "This the Doom PIMacs CLI. Try doom +pimacs --help"))

(defcli! (:root doom +pimacs mise) ()
         "Mise commands."
         (require 'doom)
         (require 'doom-start)
         (unless (hash-table-p (bound-and-true-p doom-modules))
           (doom-modules-initialize))
         (when (modulep! :pimacs mise)
           (print! "@@@@@@@@@@@ Mise is loaded")))

(defcli! (:root doom +pimacs mise install) ()
         "Install with Mise the tools needed by your config."
         (require 'doom)
         (require 'doom-start)
         (unless (hash-table-p (bound-and-true-p doom-modules))
           (doom-modules-initialize))
         (when (modulep! :pimacs mise)
           (print! "@@@@@@@@@@@ Mise is loaded")))

(defcli! (:root doom +pimacs mise update) ()
         "Update with Mise the tools needed by your config."
         (require 'doom)
         (require 'doom-start)
         (unless (hash-table-p (bound-and-true-p doom-modules))
           (doom-modules-initialize))
         (when (modulep! :pimacs mise)
           (print! "@@@@@@@@@@@ Mise is loaded")))

(provide 'pimacs/default/cli)

;;; cli.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/default/cli.el")
;; End:
