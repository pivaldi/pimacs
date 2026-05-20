;; -*- no-byte-compile: t; -*-
;;; pimacs/gptel/packages.el

;; Declare gptel here so this module is self-sufficient: enabling Doom's
;; (:tools llm) is no longer a hard prerequisite.
(package! gptel)

;; gptel-mcp is not on MELPA; pull it directly from upstream.
(package! gptel-mcp
  :recipe (:host github :repo "lizqwerscott/gptel-mcp.el"))
