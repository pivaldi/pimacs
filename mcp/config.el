;;; pimacs/mcp/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(use-package! mcp
  :defer t
  :init
  (map! :leader :desc "Open the MCP hub. #pim" "c m" #'mcp-hub)

  :config

  ;;; ── Generic helpers ────────────────────────────────────────────────────

  (defun pim/mcp-format-env-args (vars)
    "Convert env var names or KEY=VALUE strings into Docker -e arguments."
    (apply #'append
           (mapcar (lambda (var)
                     (if (string-match-p "=" var)
                         ;; It's already a static KEY=VALUE string, pass it directly
                         (list "-e" var)
                       ;; It's just a KEY, fetch it dynamically from Emacs environment
                       (list "-e" (format "%s=%s" var (or (getenv var) "")))))
                   vars)))

  (defun pim/mcp-format-volume-args (volumes)
    "Convert volume specs into a flat list of Docker -v arguments.
Specs starting with `~' or `/' are expanded as host paths; everything
else is passed through as a named volume."
    (apply #'append
           (mapcar (lambda (vol)
                     (let* ((parts (split-string vol ":"))
                            (host (car parts)))
                       (if (or (string-prefix-p "~" host)
                               (string-prefix-p "/" host))
                           (list "-v" (mapconcat #'identity
                                                 (cons (expand-file-name host) (cdr parts))
                                                 ":"))
                         (list "-v" vol))))
                   volumes)))

  (defun pim/mcp-project-root ()
    "Return the current project root, falling back to `default-directory'."
    (or (and (fboundp 'doom-project-root) (doom-project-root))
        default-directory))

  ;;; ── Server entry factories ─────────────────────────────────────────────

  (cl-defun pim/mcp-make-docker-server (name image &key
                                             args
                                             volumes
                                             env-vars
                                             mount-project
                                             extra-docker-args)
    "Build an `mcp-hub-servers' entry NAME running IMAGE via `docker run -i --rm'."
    (let* (;; directory-file-name safely removes the trailing slash Docker hates
           (root (directory-file-name (expand-file-name (pim/mcp-project-root))))
           (project-mount (when mount-project
                            (list "-v" (format "%s:/workspace" root)
                                  "-w" "/workspace"))))
      `(,name . (:command "docker"
                 :args ,(append (list "run" "-i" "--rm")
                                project-mount
                                (pim/mcp-format-volume-args (or volumes '()))
                                (pim/mcp-format-env-args (or env-vars '()))
                                (or extra-docker-args '())
                                (list image)
                                (or args '()))))))

  (cl-defun pim/mcp-make-stdio-server (name command &key args)
    "Build an `mcp-hub-servers' entry NAME running COMMAND directly via stdio."
    `(,name . (:command ,command :args ,(or args '()))))

  ;;; ── Project-aware registry ─────────────────────────────────────────────

  (defvar pim/mcp-server-builders '()
    "Alist of (NAME . THUNK) for servers whose entry depends on the project.
Each THUNK is re-evaluated before every `mcp-hub' invocation so that
project-relative paths track the active project.")

  (defun pim/mcp-register-server (entry &optional rebuild-thunk)
    "Add ENTRY to `mcp-hub-servers'.
If REBUILD-THUNK is non-nil, register it so the entry is refreshed
on every `mcp-hub' call (used for project-aware servers)."
    (after! mcp-hub
      (setf (alist-get (car entry) mcp-hub-servers nil nil #'equal)
            (cdr entry))
      (when rebuild-thunk
        (setf (alist-get (car entry) pim/mcp-server-builders nil nil #'equal)
              rebuild-thunk))))

  (advice-add 'mcp-hub :before
              (lambda (&rest _)
                (dolist (cell pim/mcp-server-builders)
                  (setf (alist-get (car cell) mcp-hub-servers nil nil #'equal)
                        (cdr (funcall (cdr cell)))))))

  ;;; ── Per-project auto-start ─────────────────────────────────────────────

  (defcustom pim-mcp-auto-start-on-project-switch nil
    "If non-nil, start matching MCP servers when switching project."
    :type 'boolean
    :group 'mcp)

  (defcustom pim-mcp-start-server-function 'mcp-hub-start-server
    "Function used to start an MCP server by name.
Different versions of mcp.el expose different entry points; override
this if your build uses a different one (e.g. `mcp-connect-server')."
    :type 'function
    :group 'mcp)

  (defvar pim/mcp-project-detectors '()
    "Alist of (SERVER-NAME . PREDICATE).
PREDICATE is a thunk called in the new project root after switching
projects; if it returns non-nil, SERVER-NAME is started via
`pim-mcp-start-server-function'.")

  (defun pim/mcp-register-project-detector (name predicate)
    "Register PREDICATE as the auto-start gate for MCP server NAME."
    (setf (alist-get name pim/mcp-project-detectors nil nil #'equal) predicate))

  (defun pim/mcp-project-auto-start ()
    "Start every MCP server whose project predicate matches the current project."
    (when (and pim-mcp-auto-start-on-project-switch
               (fboundp pim-mcp-start-server-function))
      (let ((default-directory (pim/mcp-project-root)))
        (dolist (cell pim/mcp-project-detectors)
          (let ((name (car cell))
                (pred (cdr cell)))
            (when (and (assoc name (bound-and-true-p mcp-hub-servers))
                       (ignore-errors (funcall pred)))
              (condition-case err
                  (funcall pim-mcp-start-server-function name)
                (error (message "[mcp] failed to start %s: %S" name err)))))))))

  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook #'pim/mcp-project-auto-start))

  ;;; ── +gitnexus ──────────────────────────────────────────────────────────

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
      "The internal mounting path for the GitNexus container."
      :type 'string
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-env-vars
      '("GEMINI_API_KEY" "ANTHROPIC_API_KEY" "MISE_QUIET=1" "MISE_GLOBAL_ONLY=1" "LOG_LEVEL=error")
      "List of host environment variables to pass to the GitNexus container."
      :type '(repeat string)
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-docker-volumes
      '("~/.gemini:/home/gemini/.gemini"
        "~/.claude:/home/gemini/.claude"
        "~/.claude.json:/home/gemini/.claude.json"
        "~/.gitnexus:/home/gemini/.gitnexus")
      "Volume mappings for the GitNexus container.
Format is \"host_path_or_named_volume:container_path\".
Paths starting with `~' or `/' are expanded automatically."
      :type '(repeat string)
      :group 'mcp)

    (defcustom pim-mcp-gitnexus-project-markers
      '(".gitnexus" "gitnexus.config.json")
      "Files or directories whose presence at the project root indicates
that GitNexus has indexed the project. Used by the per-project
auto-start to avoid spinning up the container in unrelated repos."
      :type '(repeat string)
      :group 'mcp)

    (defun pim/mcp-gitnexus-indexed-p ()
      "Return non-nil if the current project looks GitNexus-indexed."
      (let ((root (pim/mcp-project-root)))
        (or (cl-some (lambda (m) (file-exists-p (expand-file-name m root)))
                     pim-mcp-gitnexus-project-markers)
            (file-directory-p
             (expand-file-name (file-name-nondirectory (directory-file-name root))
                               "~/.gitnexus")))))

    (defun pim/mcp-gitnexus-server-entry ()
      "Return the GitNexus entry for the current project."
      (if pim-mcp-gitnexus-enable-docker
          (pim/mcp-make-docker-server
           "gitnexus" pim-mcp-gitnexus-docker-image
           :args (list "gitnexus" "mcp")
           :volumes pim-mcp-gitnexus-docker-volumes
           :env-vars pim-mcp-gitnexus-env-vars
           :mount-project t) ;; <-- Cleaned up! No more :workspace or :workdir
        (pim/mcp-make-stdio-server "gitnexus" "gitnexus" :args (list "mcp"))))

    (pim/mcp-register-server (pim/mcp-gitnexus-server-entry)
                             #'pim/mcp-gitnexus-server-entry)
    (pim/mcp-register-project-detector "gitnexus" #'pim/mcp-gitnexus-indexed-p))

  ;;; ── +filesystem ────────────────────────────────────────────────────────

  (when (modulep! +filesystem)
    (defcustom pim-mcp-filesystem-npx-package "@modelcontextprotocol/server-filesystem"
      "npm package providing the filesystem MCP server."
      :type 'string
      :group 'mcp)

    (defun pim/mcp-filesystem-server-entry ()
      "Return the filesystem MCP entry rooted at the current project."
      (pim/mcp-make-stdio-server
       "filesystem" "npx"
       :args (list "-y" pim-mcp-filesystem-npx-package
                   (expand-file-name (pim/mcp-project-root)))))

    (pim/mcp-register-server (pim/mcp-filesystem-server-entry)
                             #'pim/mcp-filesystem-server-entry))

  ;;; ── +github ────────────────────────────────────────────────────────────

  (when (modulep! +github)
    (defcustom pim-mcp-github-enable-docker t
      "If non-nil, run the GitHub MCP server inside Docker."
      :type 'boolean
      :group 'mcp)

    (defcustom pim-mcp-github-docker-image "ghcr.io/github/github-mcp-server"
      "Docker image for the GitHub MCP server."
      :type 'string
      :group 'mcp)

    (defcustom pim-mcp-github-env-vars '("GITHUB_PERSONAL_ACCESS_TOKEN")
      "Host env vars forwarded to the GitHub MCP container."
      :type '(repeat string)
      :group 'mcp)

    (defun pim/mcp-github-server-entry ()
      "Return the GitHub MCP entry."
      (if pim-mcp-github-enable-docker
          (pim/mcp-make-docker-server
           "github" pim-mcp-github-docker-image
           :env-vars pim-mcp-github-env-vars)
        (pim/mcp-make-stdio-server "github" "github-mcp-server"
                                   :args (list "stdio"))))

    (pim/mcp-register-server (pim/mcp-github-server-entry)))

  ;;; ── +context7 ──────────────────────────────────────────────────────────

  (when (modulep! +context7)
    (defcustom pim-mcp-context7-npx-package "@upstash/context7-mcp"
      "npm package providing the Context7 MCP server."
      :type 'string
      :group 'mcp)

    (pim/mcp-register-server
     (pim/mcp-make-stdio-server "context7" "npx"
                                :args (list "-y" pim-mcp-context7-npx-package)))))
