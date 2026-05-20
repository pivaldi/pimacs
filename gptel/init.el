;;; pimacs/gptel/init.el -*- lexical-binding: t; -*-

;; `gptel-menu' is a transient command, not a keymap variable, so it is
;; intentionally NOT registered here.
(add-to-list 'pim-keymapname-alist
             '("gptel" . (("gptel-mode-map" . gptel))))
