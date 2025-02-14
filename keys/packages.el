;;; -*- no-byte-compile: t; -*-

(package! jump-to-prev-pos
  :recipe (:host github :repo "pivaldi/jump-to-prev-pos.el"))
(package! ace-window)

;; Don't like this package
(package! better-jumper
  :disable t)

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/keys/packages.el")
;; End:
