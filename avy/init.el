;;; pimacs/avy/init.el -*- lexical-binding: t; -*-

;; TODO : handle ::prefix flag in `pim-keys-bindings-to-refcard'
(add-to-list 'pim-keymapname-alist '("avy" . (("global-map::prefix:M-g a" . nil) ("isearch-mode-map" . nil))))

(provide 'pimacs/avy/init)
;;; init.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/avy/init.el")
;; End:
