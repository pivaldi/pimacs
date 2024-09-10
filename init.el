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
 session ;; Auto reload the last session at startup through `doom/quickload-session` (no desktop bad practice in Doom).
 avy ;; usefull key bindings for avy because Doom does not provide then.
 bm ;; Provides visible and BUFFER LOCAL bookmarks with the ability to jump forward and backward to the next bookmark.
 calendar-fr ;; French calendar.
 tramp ;;To “turn off” the backup feature for remote files and fix security issue.
 origami ;; A text folding minor mode for Emacs.

 org ;; Org mode specific configuration.
 dired ;; Dired mode specific configuration.
 lang-c
 (lang-php +php-cs-fixer) ;; Configuration for PHP coding.
 lang-lisp
 lang-make

 doc ;; Generated documentation, fundamental keys binding for example.
 )

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/init.el")
;; End:
