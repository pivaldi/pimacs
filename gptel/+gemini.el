;;; pimacs/gptel/+gemini.el -*- lexical-binding: t; -*-

;; Configure the Google Gemini backend.
;;
;; If +claude is ALSO enabled, it ran first (see load order in config.el) and is
;; already the default; this file only registers Gemini so it's selectable from
;; `gptel-menu'. With only +gemini enabled, Gemini becomes the default backend.
;;
;; Reuses the GEMINI_API_KEY already forwarded into the ai-sandbox container
;; (see mcp/config.el :: pim-mcp-gitnexus-env-vars), so the same secret powers
;; both MCP-side calls and gptel-side chats.
(let ((gemini-backend
       (gptel-make-gemini
           "Gemini"
         :stream t
         ;; Lazy lookup: only checked when a message is actually sent, so Emacs
         ;; still boots if the variable is missing.
         :key (lambda ()
                (or (getenv "GEMINI_API_KEY")
                    (error "Please set GEMINI_API_KEY in your environment to use Gemini"))))))
  (unless (modulep! +claude)
    (setq-default gptel-backend gemini-backend)))
