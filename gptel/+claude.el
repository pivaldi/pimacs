;;; pimacs/gptel/+claude.el -*- lexical-binding: t; -*-

;; Configure the Anthropic backend
(setq-default gptel-backend
              (gptel-make-anthropic
                  "Claude"
                :stream t
                ;; SECURITY & PORTABILITY: Use a lambda so the environment variable
                ;; is only checked when the user actually tries to send a message,
                ;; not when Emacs boots up!
                :key (lambda () 
                       (or (getenv "ANTHROPIC_API_KEY")
                           (error "Please set ANTHROPIC_API_KEY in your environment to use Claude")))))

;; (defun run-ephemeral-claude ()
;;   "Launch Claude Code on-demand via npx inside an Emacs vterm buffer."
;;   (interactive)
;;   (let ((vterm-buffer (vterm "*claude-ephemeral-agent*")))
;;     (with-current-buffer vterm-buffer
;;       ;; Sends the command to execute without permanent system mutation
;;       (vterm-send-string "npx @anthropic-ai/claude-code\n"))
;;     (switch-to-buffer vterm-buffer)))
