;;; pimacs/notmuch/init.el -*- lexical-binding: t; -*-

(add-to-list ' pim-keymapname-alist '("notmuch" . (("notmuch-search-mode-map" . notmuch-search-mode)
                                                   ("notmuch-tree-mode-map" . notmuch-tree-mode)
                                                   ("notmuch-show-mode-map" . notmuch-show-mode))))
