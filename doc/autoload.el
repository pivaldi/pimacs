;;; pimacs/doc/autoload.el -*- lexical-binding: t; -*-
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

(defun pim-format-to-title (str ext)
  "Return the formated string STR as title depending of EXT.
EXT, a string, can be org, md or txt."
  (cond
   ((equal ext "org") (format "#+title: %s" str))
   ((or (equal ext "md") (equal ext "txt")) (format "# %s" str))
   (t (error "%s is not a supported extension" ext))))

(defun pim-format-to-sub-title (str ext)
  "Return the formated string STR as sub-title depending of EXT.
EXT, a string, can be org, md or txt."
  (cond
   ((equal ext "org") (format "* %s" str))
   ((or (equal ext "md") (equal ext "txt")) (format "## %s" str))
   (t (error "%s is not a supported extension" ext))))

(defun pim-format-to-list-elem (str ext)
  "Return the formated string STR as list element depending of EXT.
EXT, a string, can be org, md or txt."
  (cond
   ((equal ext "org") (format "- %s" str))
   ((equal ext "md") (format "* %s" str))
   ((equal ext "txt") (format "• %s" str))
   (t (error "%s is not a supported extension" ext))))

(defun pim-format-to-bold (str ext)
  "Return the formated string STR in bold depending of EXT.
EXT, a string, can be org, md or txt."
  (cond
   ((or (equal ext "org") (equal ext "txt")) (format "*%s*" str))
   ((equal ext "md") (format "**%s**" str))
   (t (error "%s is not a supported extension" ext))))

(defun pim-format-to-inline-code (str ext)
  "Return the formated string STR as inline code depending of EXT.
EXT, a string, can be org, md or txt."
  (cond
   ((equal ext "org") (format "=%s=" str))
   ((equal ext "md") (format "`%s`" str))
   ((equal ext "txt") (format "\"%s\"" str))
   (t (error "%s is not a supported extension" ext))))


(defun pim-keys-bindings-to-refcard (keymaps-alist fname &optional all)
  "Export the PIMacs key bindings in file (org, md and txt format supported).
KEYMAPS-ALIST (list of (string . provide-symbol)) is the list of keymap names to export into
the file FNAME.
If KEYMAPS-ALIST is nil, use the variable `pim-keymapname-alist' instead."
  ;; TODO : make it interactive
  (require 'which-key)
  (let* ((which-key-max-description-length 1000)
         (line nil)
         (pim-regexp "\\(.*\\)#pim *$")
         (keyboard (if (modulep! :pimacs keys +azerty) " (with option +azerty)" ""))
         (keys '())
         (keymap nil)
         (keymapname "")
         (mapprovider nil)
         (ext (file-name-extension fname)))
    (unless keymaps-alist
      (setq keymaps-alist '())
      (dolist (km pim-keymapname-alist)
        (setq keymaps-alist (append keymaps-alist (cdr km)))
        ))
    (with-temp-file fname
      (insert (pim-format-to-title (format "PIMacs bindings%s\n\n" keyboard) ext))
      (insert (format "%s, it is auto-generated by PIMacs.\n" (pim-format-to-bold "DO NOT EDIT THIS FILE" ext)))
      (dolist (keymap-assoc keymaps-alist)
        (setq keymapname (car keymap-assoc))
        (setq mapprovider (cdr keymap-assoc))
        (when mapprovider (require mapprovider))
        (unless (stringp keymapname) (error "keymap name `%s' is not a string." keymapname))
        (setq keymap (symbol-value (intern keymapname)))
        (unless (keymapp keymap) (error "keymap %s is not a valid keymap." keymapname))
        (insert (format "\n%s\n\n" (pim-format-to-sub-title (format "Keymap %s" keymapname) ext)))
        (setq keys (which-key--get-bindings nil keymap nil t))
        (dolist (key keys)
          (setq line (pim-format-to-list-elem
                      (apply #'format "%s%s%s" (pim-format-to-inline-code (pop key) ext) key) ext))
          ;; (print! line)
          (when (or all (string-match-p pim-regexp line))
            (insert (concat line "\n"))
            ))))))

;;;###autoload
(defun pim/modules-key-bindings-to-refcard (modulenames fname)
  "Export the PIMacs key bindings in file (org, md and txt format supported).
MODULENAMES (list of string) is the list of PIMacs modules names that create key bindings.
Export the refcard to FNAME.
If MODULENAMES is nil, use the variable `pim-keymapname-alist' instead."
  (interactive
   (list
    (completing-read-multiple
     (format "Binding module (separator regexp is comma) : " crm-separator)
     (append (list "All PIMacs Modules)")
             (mapcar (lambda (m) (format ":pimacs %s" (car m))) pim-keymapname-alist))
     nil t nil nil)
    (car (find-file-read-args "Export to file : " nil))))
  (let ((allkeymaps-alist '()))
    (when modulenames
      (catch 'exit
        (dolist (mname modulenames)
          (let* ((mrealname (string-trim mname ":pimacs "))
                 (keymaps-alist (cdr (assoc mrealname pim-keymapname-alist))))
            (if keymaps-alist
                (setq allkeymaps-alist (append allkeymaps-alist keymaps-alist))
              (progn
                (setq allkeymaps-alist nil)
                (throw 'exit allkeymaps-alist)))))))
    (pim-keys-bindings-to-refcard allkeymaps-alist fname)))

(defun pim-keystring-kbd-consolidate (keystr)
  "Consolidate KEYSTR, a string key representation, to be read by `kbd'."
  (string-replace
   ">>" ">"
   (string-replace
    "<<" "<"
    (s-replace-regexp "\\(\[a-zA-Z0-9]\\{2,\\}\\)" "<\\1>" keystr)))
  )

(defun pim-which-key-insert-bindings-recursively-org (prefix level)
  "Insert recursively all key bindings with prefix PREFIX with org formating.
LEVEL represent the deep of the heading."
  (require 'which-key)
  (let* ((which-key-max-description-length 1000)
         (keys (which-key--get-bindings (kbd prefix)))
         (section-s (make-string level ?*)))
    (dolist (key keys)
      (let ((prefixn (s-trim (format "%s %s" prefix (pim-keystring-kbd-consolidate (pop key))))))
        (insert (apply #'format "%s =%s=%s%s\n\n" section-s prefixn key))
        (pim-which-key-insert-bindings-recursively-org prefixn (+ 1 level))
        )
      )
    )
  )

;;;###autoload
(defun pim/which-key-export-bindings-recursively-to-file (prefix file-name)
  "Export bindings recursively from PREFIX into file FILE-NAME with org formatting.
PREFIX should be a string suitable for `kbd'."
  (interactive "sPrefix: \nF")
  (require 'toc-org)
  (with-temp-file file-name
    (point-max)
    (save-excursion
      (if (equal prefix "")
          (insert (pim-format-to-title "All Fundamental Key Bindings\n\n" "org"))
        (insert (pim-format-to-title (format "Fundamental Key Bindings Prefixed by %s\n\n" prefix) "org")))
      (insert (format "%s, it is auto-generated by PIMacs.\n" (pim-format-to-bold "DO NOT EDIT THIS FILE" "org")))
      (insert "* Table of Content :TOC_1:\n\n")
      (insert "* Table of Content :TOC_1:\n\n")
      (pim-which-key-insert-bindings-recursively-org prefix 1)
      (toc-org-insert-toc))))

;; *=================================================================*
;; *.......Functions to generate all the key bindings refcards.......*
;; *=================================================================*

;;;###autoload
(defun pim-generate-all-fundamental-key-bindings ()
  "PIMacs internal use only. User should use `pim/which-key-export-bindings-recursively-to-file' instead."
  (let
      ((fname "")
       (prefixes pim-doc-key-binding-prefixes-to-export))
    (dolist (prefix prefixes)
      (setq fname (format "%s/doom-refcard-%s.org" (doom-module-locate-path :pimacs 'doc) prefix))
      (pim/which-key-export-bindings-recursively-to-file prefix fname))))

;;;###autoload
(defun pim-generate-all-modules-key-bindings-refcards ()
  "PIMacs internal use only. User should use `pim/modules-key-bindings-to-refcard' instead."
  (interactive)
  (let ((fname "")
        (keymapnames nil)
        (modulename nil)
        (module nil))
    (general-override-mode +1)
    (dolist (all (list nil t))
      (dolist (km pim-keymapname-alist)
        (progn
          (setq
           modulename (car km)
           keymapnames (cdr km)
           module (intern modulename)
           fname (concat (doom-module-locate-path :pimacs module)
                         (format "/%s-key-bindings-refcard.md" (if all "all" "pimacs"))))
          (print! (green (format "Generating %s key binding from module %s…" keymapnames modulename)))
          (pim-keys-bindings-to-refcard keymapnames fname all)
          (print! (green (format "…wrote into %s" fname))))))))

(provide 'pimacs/doc/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/doc/autoload.el")
;; End:
