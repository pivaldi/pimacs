;;; pimacs/mcp/init.el -*- lexical-binding: t; -*-

;; PIMacs adds no bindings to `mcp-hub-mode-map' itself, but the refcard now
;; always emits a "Default Mode Key Bindings" section, so the upstream
;; cheat-sheet (s start, r restart, q quit, g refresh, …) shows up there.
;; `:warmup' is still required: upstream installs the bindings inside the
;; `define-derived-mode mcp-hub-mode' body, so the map stays empty until the
;; mode runs at least once. The generator activates the mode in a temp buffer.
(add-to-list 'pim-keymapname-alist
             '("mcp" . (("mcp-hub-mode-map" mcp-hub :warmup mcp-hub-mode))))
