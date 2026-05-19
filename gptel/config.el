;;; pimacs/gptel/config.el -*- lexical-binding: t; -*-

;;; gptel Configuration
(use-package! gptel
  :defer t
  :config
  (when (modulep! +claude)
    (load! "+claude")
    ))

;;; The MCP Bridge
(use-package! gptel-mcp
  :after (gptel mcp)
  :config
  ;; Automatically tell gptel to look for available MCP tools (like GitNexus)
  (gptel-mcp-tools-mode 1))
