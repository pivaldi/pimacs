;;; pimacs/treesit/doctor.el -*- lexical-binding: t; -*-

(when (< emacs-major-version 29)
  (error! "Emacs 29+ is needed"))

(unless (fboundp 'treesit-install-language-grammar)
  (error! "Native treesit module *no*t found. Complie Emacs with configuration option --with-tree-sitter"))
