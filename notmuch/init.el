;;; pimacs/notmuch/init.el -*- lexical-binding: t; -*-

;; Each *-mode-map lives in the file matching its `provide' form:
;;   notmuch-search-mode-map → notmuch.el     (provides `notmuch')
;;   notmuch-tree-mode-map   → notmuch-tree.el (provides `notmuch-tree')
;;   notmuch-show-mode-map   → notmuch-show.el (provides `notmuch-show')
(add-to-list 'pim-keymapname-alist '("notmuch" . (("notmuch-search-mode-map" notmuch)
                                                  ("notmuch-tree-mode-map" notmuch-tree)
                                                  ("notmuch-show-mode-map" notmuch-show))))
