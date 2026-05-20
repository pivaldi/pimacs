;;; pimacs/gptel/config.el -*- lexical-binding: t; -*-

;;; gptel Configuration
(use-package! gptel
  :defer t
  :config
  ;; Local in-process tools (eval_elisp, read_buffer, list_buffers,
  ;; project_grep). Backend-agnostic — usable by any gptel backend.
  (load! "+tools")
  ;; Backend selection. Claude is loaded first so it wins `setq-default' if both
  ;; flags are on; Gemini then registers itself and stays selectable via
  ;; `gptel-menu'.
  (when (modulep! +claude)
    (load! "+claude"))
  (when (modulep! +gemini)
    (load! "+gemini")))

;;; The MCP Bridge
;; `gptel-mcp' (lizqwerscott/gptel-mcp.el) bridges mcp.el servers into gptel as
;; callable tools. There is no auto-mode: start the server you need from
;; `mcp-hub', then invoke `gptel-mcp-dispatch' to register its tools with gptel.
(use-package! gptel-mcp
  :after (gptel mcp)
  :commands (gptel-mcp-dispatch
             gptel-mcp-start-all-server-and-register
             gptel-mcp-activate-all-tool
             gptel-mcp-deactivate-all-tool)
  :init
  (map! :leader :desc "gptel-mcp dispatch #pim" "c M" #'gptel-mcp-dispatch))
