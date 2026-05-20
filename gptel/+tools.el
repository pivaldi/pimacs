;;; pimacs/gptel/+tools.el -*- lexical-binding: t; -*-

;;; Local gptel tools — in-process complements to MCP servers.
;;
;; These run inside the running Emacs (no Docker round-trip) and are
;; useful for everyday "look around" tasks. The model can mix them
;; freely with MCP tools registered by `gptel-mcp'.

(defun pim/gptel-tool-eval-elisp (form)
  "Evaluate FORM (an elisp source string) and return the printed result."
  (condition-case err
      (let ((value (eval (read form) t)))
        (format "%S" value))
    (error (format "Error: %S" err))))

(defun pim/gptel-tool-read-buffer (name)
  "Return the full contents of the live buffer NAME."
  (if-let ((buf (get-buffer name)))
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max)))
    (format "No buffer named %s" name)))

(defun pim/gptel-tool-list-buffers ()
  "Return live buffers as TAB-separated `name<TAB>file' lines."
  (mapconcat (lambda (b)
               (format "%s\t%s"
                       (buffer-name b)
                       (or (buffer-file-name b) "")))
             (buffer-list)
             "\n"))

(defun pim/gptel-tool-project-grep (pattern)
  "Search the current project for PATTERN with ripgrep (falls back to grep)."
  (let* ((root (or (and (fboundp 'doom-project-root) (doom-project-root))
                   default-directory))
         (cmd (if (executable-find "rg")
                  (format "rg --no-heading --line-number --color=never -e %s %s"
                          (shell-quote-argument pattern)
                          (shell-quote-argument root))
                (format "grep -RIn --color=never -e %s %s"
                        (shell-quote-argument pattern)
                        (shell-quote-argument root)))))
    (with-output-to-string
      (with-current-buffer standard-output
        (call-process-shell-command cmd nil t)))))

(gptel-make-tool
 :name "eval_elisp"
 :function #'pim/gptel-tool-eval-elisp
 :description "Evaluate an Emacs Lisp expression and return its printed result."
 :args (list '(:name "form"
               :type string
               :description "Elisp form to evaluate, as text."))
 :category "emacs"
 :confirm t)

(gptel-make-tool
 :name "read_buffer"
 :function #'pim/gptel-tool-read-buffer
 :description "Return the entire contents of a live Emacs buffer by name."
 :args (list '(:name "name"
               :type string
               :description "Buffer name."))
 :category "emacs")

(gptel-make-tool
 :name "list_buffers"
 :function #'pim/gptel-tool-list-buffers
 :description "List live buffer names with their backing file (if any)."
 :args nil
 :category "emacs")

(gptel-make-tool
 :name "project_grep"
 :function #'pim/gptel-tool-project-grep
 :description "Search the current project for a regex with ripgrep (or grep)."
 :args (list '(:name "pattern"
               :type string
               :description "Regex pattern to search for."))
 :category "project")
