;;; pimacs/lang-typst/autoload.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun pim-typst-tools-install-globally (&optional trusted)
  "Install globally Typst tools if pimacs/mise module is available.
If TRUSTED is t, auto-trust the mise.toml file else ask to the user."
  (interactive)
  (if (modulep! :pimacs mise)
      (let ((installed (pim-mise-install (doom-module-expand-path '(:pimacs . lang-typst)) trusted)))
        (unless (executable-find "typst")
          (pim-mise-use-global "typst@latest"))
        (unless (executable-find "tinymist")
          (pim-mise-use-global "tinymist@latest"))
        (message (if installed "done" "failed")))
    (warn "pimacs/mise is not loaded so Typst tools can not be installed.")))

(provide 'pimacs/lang-typst/autoload)
;;; autoload.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-typst/autoload.el")
;; End:
