;;; pimacs/lang-typst/config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; PIMacs configuration for Typst

;;; Code:

(if (not (executable-find "typst"))
    (add-to-list 'pim-error-msgs "Please install typst. See https://typst.app/docs/installation/\nIf you use pimacs/mise for the first time, restarting emacs is needed."))

(when (modulep! +lsp)
  (if (not (executable-find "tinymist"))
      (add-to-list 'pim-error-msgs "Please install tinymist for LSP support. See https://myriad-dreamin.github.io/tinymist/\nIf you use pimacs/mise for the first time, restarting emacs is needed.")))

(use-package! typst-ts-mode
  :defer t
  :mode "\\.typ\\'"
  :config
  (when (modulep! +lsp)
    (add-hook 'typst-ts-mode-hook #'lsp!)))

(use-package! typst-preview
  :defer t
  :after typst-ts-mode
  :config
  (setq typst-preview-browser "default")
  (map! :map typst-ts-mode-map
        :localleader
        :desc "Typst preview" "p" #'typst-preview-mode))

(provide 'pimacs/lang-typst/config)
;;; config.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-typst/config.el")
;; End: