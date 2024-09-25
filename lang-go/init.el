;;; pimacs/lang-go/init.el -*- lexical-binding: t; -*-

(if (fboundp 'go-ts-mode)
    (add-to-list ' pim-keymapname-alist '("lang-go" . (("go-ts-mode-map" . go-ts-mode)
                                                       ("go-mode-map" . go-mode))))
  (add-to-list ' pim-keymapname-alist '("lang-go" . (("go-mode-map" . go-mode)))))
