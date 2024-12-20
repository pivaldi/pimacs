;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom PIMacs modules are enabled and what
;; order they load in.

(doom!
 :pimacs
 (default +azerty) ;; PIMacs default Emacs configuration on top of Doom with azerty keyboard type.
 functions ;; Useful functions. Must be loaded for the keys module/
 aliases ;; Define aliases starting with `_`. The most useful are `_rb` and `_sir`.
 theme ;; Define some faces and load Zenburn Theme. See the `README.md` in the directory `pimacs/theme`.
 keys ;; Define the PIMacs keys binding. See the `refcard.md`.
 session ;; Rotated backup of session and workspaces and auto reload the more recent saved session at startup.
 avy ;; usefull key bindings for avy because Doom does not provide then.
 bm ;; Provides visible and BUFFER LOCAL bookmarks with the ability to jump forward and backward to the next bookmark.
 calendar-fr ;; French calendar.
 tramp ;;To “turn off” the backup feature for remote files and fix security issue.
 origami ;; A text folding minor mode for Emacs.
 pairing ;; Enhanced configuration of smartparens (this module need Doom default module loaded whith +smartparens)
 org ;; Org mode specific configuration.
 dired ;; Dired mode specific configuration.
 treesit ;; Automatically install and use tree-sitter major modes in Emacs 29+
 corfu ;; Configure Corfu : remove automatic completion, remove TAB key for next candidate.
 (lsp +doc) ;; Configures lsp and lsp-ui-doc (option +doc)
 lang-c
 (lang-php +php-cs-fixer) ;; Configuration for PHP coding.
 lang-lisp
 (lang-go +lsp)
 lang-make

 notmuch ;; notmuch and mbsync config
 doc ;; Generated documentation, fundamental keys binding for example.
 )

;; (when (file-directory-p (doom-module-locate-path '(:pi . notmuch))) ;; My private configurations
(doom!
 :pi
 notmuch ;; My personnal notmuch overwrote data
 )

;; )

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/init.el")
;; End:
