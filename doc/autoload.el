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


(defun pim-keys-bindings-to-refcard (keymaps-alist &optional all)
  "Export the PIMacs key bindings in file (org, md and txt format supported).
KEYMAPS-ALIST (list of (keymap-name:string . provide:symbol)) is the list of keymap names to export into
the file FNAME.
keymap-name can specify a list of prefix restriction with the forl \"global-map::prefix:M-g a::prefix:C-c x etc…\".
If KEYMAPS-ALIST is nil, use the variable `pim-keymapname-alist' instead."
  ;; TODO : make it interactive
  (let* ((keymap nil)
         (keymapname "")
         (keymapname-with-prefixes "")
         (mapprovider nil)
         (prefixes '())
         (prefix "")
         (result ""))
    (unless keymaps-alist
      (setq keymaps-alist '())
      (dolist (km pim-keymapname-alist)
        (setq keymaps-alist (append keymaps-alist (cdr km)))
        ))
    (dolist (keymap-assoc keymaps-alist)
      (setq keymapname-with-prefixes (car keymap-assoc))
      (setq prefixes (split-string keymapname-with-prefixes "::prefix:"))
      (setq keymapname (pop prefixes))
      (setq mapprovider (cdr keymap-assoc))
      (when mapprovider (require mapprovider))
      (unless (stringp keymapname) (error "keymap name '%s' is not a string." keymapname))
      (setq keymap (symbol-value (intern keymapname)))
      (unless (keymapp keymap) (error "keymap '%s' is not a valid keymap." keymapname))
      (while ;; while loop exit cond is "prefixes nil".
          (progn
            (setq prefix (pop prefixes)) ;; prefixes decreasing.
            ;; TODO : use a temp-buffer instead !!
            (setq result (concat result (pim-which-key-get-bindings-recursively-org keymap prefix 1 nil (not all))))
            prefixes)))
    result
    ))

;;;###autoload
(defun pim/modules-key-bindings-to-refcard (modulenames fname)
  "Export the PIMacs key bindings in file (org, md and txt format supported).
MODULENAMES (list of string) is the list of PIMacs modules names that create key
bindings.
Export the refcard to FNAME.
If MODULENAMES is nil, use the variable `pim-keymapname-alist' instead."
  (interactive
   (list
    (completing-read-multiple
     "Binding module (separator regexp is %s) : "
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
    (with-temp-file fname
      (insert (pim-format-to-title
               (format
                "PIMacs/%s Key Bindings with Keyboard Option \"%s\"\n\n"
                (mapconcat 'identity modulenames ",") pim-keyboard-type) "org"))
      (insert "This reafcard is auto-generated by [[https://github.com/pivaldi/pimacs][PIMacs]].\n\n")
      (insert (pim-keys-bindings-to-refcard allkeymaps-alist)))))

(defun pim-keystring-kbd-consolidate (keystr)
  "Consolidate KEYSTR, a string key representation, to be read by `kbd'."
  (string-replace
   ">>" ">"
   (string-replace
    "<<" "<"
    (s-replace-regexp "\\(\[a-zA-Z0-9]\\{2,\\}\\)" "<\\1>" keystr)))
  )

(defun pim-function-check (x)
  "Test an object is a function.
Comes from https://stackoverflow.com/a/38173593"
  (if (symbolp x)
      (fboundp x)
    (functionp x)))

(defun pim-downcase-first-char (&optional string)
  "Donwcase only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (downcase first-char) rest-str))))

(defun pim-which-key-get-bindings-recursively-org (keymap prefix level &optional subkeys restrict-to-pim)
  "Insert recursively all key bindings with prefix PREFIX with org formating.
LEVEL represent the deep of the heading."
  (require 'which-key)
  (when (not prefix) (setq prefix ""))
  (let* ((which-key-max-description-length 1000)
         (keys (if subkeys subkeys (which-key--get-bindings (kbd prefix) keymap)))
         (pim-regexp "\\(.*\\)#pim *$")
         (isPrefix (not (null keys)))
         (result "")
         (keymapname (help-fns-find-keymap-name keymap))
         (prefixf (if (equal prefix "") "" (format "Prefix =%s=" prefix)))
         (prekeymapf (if (equal prefix "") "" " on "))
         (keymapf (if keymap (format "%sKeymap =%s=" prekeymapf keymapname) ""))
         (level1part ""))
    (when (and (equal prefix "") (not keymap)) (setq prefixf "No Prefix on No Keymap"))
    (setq level1part (if (or nil (eq 1 level)) (format "* %s%s\n" prefixf keymapf) ""))
    (dolist (key keys)
      (let* ((prefixn (s-trim (format "%s %s" prefix (pim-keystring-kbd-consolidate (pop key)))))
             (sub-keys (which-key--get-bindings (kbd prefixn) keymap))
             (isSubKeyPrefix (not (null sub-keys)))
             (section-s (if isSubKeyPrefix (make-string (+ 1 level) ?*) "-"))
             (keydesc (apply #'format "%s" (cdr key)))
             (callable (and (not (string-match " " keydesc)) (pim-function-check (intern keydesc))))
             (docstring "")
             (line "")
             (separator " : "))
        (when callable
          (progn
            (setq docstring (helpful--docstring (intern keydesc) t) )
            (if docstring
                (setq docstring
                      (format " : %s" (pim-downcase-first-char (s-replace-regexp "\n.*" "" docstring))))
              (setq docstring " (not described)"))
            (setq keydesc (format "=%s=" keydesc))
            (setq separator " calls ")))
        (setq line (format "%s %s=%s=%s%s%s\n" section-s (if isSubKeyPrefix "Prefix " "") prefixn separator keydesc docstring))
        (when (or (not restrict-to-pim) (string-match-p pim-regexp line))
          (if (and isPrefix (not isSubKeyPrefix))
              (setq level1part (concat level1part line))
            (setq result (concat result line)))
          (when isSubKeyPrefix
            (setq result
                  (concat
                   result (pim-which-key-get-bindings-recursively-org keymap prefixn (+ 1 level) sub-keys)))))
        )
      )
    (concat level1part result)
    )
  )

;;;###autoload
(defun pim/which-key-export-bindings-recursively-to-file (keymap prefix file-name)
  "Export bindings recursively from PREFIX into file FILE-NAME with org formatting.
PREFIX should be a string suitable for `kbd'."
  ;; (interactive "sPrefix: \nF")
  (interactive (list
                (symbol-value (which-key--read-keymap))
                (read-string "Key Prefix: ")
                (car (find-file-read-args "Export to file : " nil))))
  (require 'toc-org)
  (with-temp-file file-name
    (point-max)
    (save-excursion
      (let
          ((keymapname (pim-format-to-inline-code (help-fns-find-keymap-name keymap) "org"))
           (prefixstm (if (equal prefix "")
                          ""
                        (format " Prefixed by %s" (pim-format-to-inline-code prefix "org"))))
           (title ""))
        (if (and (equal prefix "") (not keymap))
            (setq title "Key Bindings Without Prefix Nor Keymap\n\n")
          (if keymap
              (setq title (format  "Key Bindings of Keymap %s%s\n\n" keymapname prefixstm))
            (setq title (format  "Key Bindings%s Without Keymap\n\n" prefixstm))))
        (insert (pim-format-to-title title "org"))
        (insert "This reafcard is auto-generated by [[https://github.com/pivaldi/pimacs][PIMacs]].\n")
        (insert "* Table of Content :TOC_2:\n\n")
        (insert (pim-which-key-get-bindings-recursively-org keymap prefix 1))
        (toc-org-insert-toc)))))

;; *=================================================================*
;; *.......Functions to generate all the key bindings refcards.......*
;; *=================================================================*

;;;###autoload
(defun pim-generate-all-fundamental-key-bindings ()
  "PIMacs internal use only.
User should use `pim/which-key-export-bindings-recursively-to-file' instead."
  (let
      ((fname "")
       (prefixes pim-doc-key-binding-prefixes-to-export))
    (dolist (prefix prefixes)
      (setq fname (format "%s/doom-refcard-%s.org" (doom-module-locate-path :pimacs 'doc) prefix))
      (pim/which-key-export-bindings-recursively-to-file nil prefix fname))))

;;;###autoload
(defun pim-generate-all-keymaps ()
  "PIMacs internal use only."
  (let
      ((fname "")
       (keymaps-alist pim-doc-keymaps-to-export)
       (keymap nil)
       (keymapname "")
       (module nil))
    (dolist (kalist keymaps-alist)
      (setq keymapname (car kalist))
      (setq module (cdr kalist))
      (when module (require module))
      (setq keymap (symbol-value (intern keymapname)))
      (setq fname (format "%s/doom-refcard-%s.org" (doom-module-locate-path :pimacs 'doc) (s-replace-regexp "[^a-zA-Z0-1_.-]" "_" keymapname)))
      (pim/which-key-export-bindings-recursively-to-file keymap "" fname))))

;;;###autoload
(defun pim-generate-all-modules-key-bindings-refcards ()
  "PIMacs internal use only.
User should use `pim/modules-key-bindings-to-refcard' instead."
  (interactive)
  (let ((fname "")
        (keymapnames nil)
        (modulename nil)
        (module nil))
    (general-override-mode +1)
    (dolist (km pim-keymapname-alist)
      (progn
        (setq
         modulename (car km)
         keymapnames (cdr km)
         module (intern modulename)
         fname (concat (doom-module-locate-path :pimacs module)
                       (format "/%s-key-bindings-refcard.org" modulename)))
        (with-temp-file fname
          (insert (pim-format-to-title
                   (format
                    "PIMacs/%s Key Bindings with Keyboard Option \"%s\"\n\n"
                    modulename pim-keyboard-type) "org"))
          (insert "This reafcard is auto-generated by [[https://github.com/pivaldi/pimacs][PIMacs]].\n\n")
          (insert (pim-keys-bindings-to-refcard keymapnames))
          )))))

(provide 'pimacs/doc/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/doc/autoload.el")
;; End:
