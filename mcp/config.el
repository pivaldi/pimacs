;;; pimacs/mcp/config.el -*- lexical-binding: t; -*-

(use-package! mcp
  :defer t
  :init
  (map! :leader "c m" #'mcp-hub)

  :config
  (when (modulep! +gitnexus)
    (defcustom pim-mcp-gitnexus-enable-docker t
      "If non-nil, run GitNexus inside a Docker container.
If nil, run it locally via npx."
      :type 'boolean
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-docker-image "ai-sandbox:latest"
      "The Docker image to use for the GitNexus MCP server."
      :type 'string
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-docker-workspace "/workspace"
      "The internal mounting path for the Docker container."
      :type 'string
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-env-vars '("GEMINI_API_KEY" "ANTHROPIC_API_KEY")
      "List of host environment variables to pass to the container."
      :type '(repeat string)
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-docker-volumes
      '("~/.gemini:/home/gemini/.gemini"
        "~/.claude:/home/gemini/.claude"
        "~/.claude.json:/home/gemini/.claude.json"
        "~/.gitnexus:/home/gemini/.gitnexus")
      "List of volume mappings for the Docker container.
Format is \"host_path_or_named_volume:container_path\".
File paths starting with `~` or `/` will be automatically expanded."
      :type '(repeat string)
      :group 'mcp)

    (defun pim/mcp-format-env-args (vars)
      "Convert a list of env var strings into a flat list of Docker -e arguments."
      (apply #'append
             (mapcar (lambda (var)
                       (list "-e" (format "%s=%s" var (getenv var))))
                     vars)))

    (defun pim/mcp-format-volume-args (volumes)
      "Convert a list of volume strings into a flat list of Docker -v arguments."
      (apply #'append
             (mapcar (lambda (vol)
                       (let* ((parts (split-string vol ":"))
                              (host (car parts)))
                         (if (or (string-prefix-p "~" host)
                                 (string-prefix-p "/" host))
                             ;; Expand the host path and re-join with the container path
                             (list "-v" (mapconcat #'identity (cons (expand-file-name host) (cdr parts)) ":"))
                           ;; It's a named volume, just use it as-is
                           (list "-v" vol))))
                     volumes)))

    (defun pim/mcp-gitnexus-server-entry ()
      "Return the mcp-hub-servers entry for GitNexus for the current project."
      (let ((project-dir (or (doom-project-root) default-directory)))
        (if pim-mcp-gitnexus-enable-docker
            `("gitnexus" .
              (:command "docker"
               :args ,(append (list "run" "-i" "--rm"
                                    "-v" (format "%s:%s" project-dir pim-mcp-gitnexus-docker-workspace)
                                    "-w" pim-mcp-gitnexus-docker-workspace)
                              (pim/mcp-format-volume-args pim-mcp-gitnexus-docker-volumes)
                              (pim/mcp-format-env-args pim-mcp-gitnexus-vars-env)
                              (list pim-mcp-gitnexus-docker-image "gitnexus" "mcp"))))
          '("gitnexus" . (:command "gitnexus" :args ("mcp"))))))

    ;; Refresh the gitnexus entry with the current project root each time mcp-hub starts.
    (advice-add 'mcp-hub :before
                (lambda (&rest _)
                  (setf (alist-get "gitnexus" mcp-hub-servers nil nil #'equal)
                        (cdr (pim/mcp-gitnexus-server-entry)))))

    (after! mcp-hub
      (add-to-list 'mcp-hub-servers (pim/mcp-gitnexus-server-entry)))))
