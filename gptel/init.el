;;; pimacs/gptel/init.el -*- lexical-binding: t; -*-

;; This module deliberately does NOT register `gptel-mode-map' in
;; `pim-keymapname-alist': PIMacs adds no bindings to it (the main entry point
;; is the `gptel-menu' transient, not a keymap variable), so the generated
;; refcard would always be empty. Leader-level bindings — `SPC c M' for
;; `gptel-mcp-dispatch' — are tagged with `#pim' and surface in the keys module
;; refcard instead.
