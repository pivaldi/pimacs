;; -*- no-byte-compile: t; -*-
;;; pimacs/lang-go/packages.el

(package! flycheck-golangci-lint
  :recipe (:host github :repo "pivaldi/flycheck-golangci-lint")
  :pin "9fc6e0ef4e3f27308297fe6f046c8fdbb1625dcc")
