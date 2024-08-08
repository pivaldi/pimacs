;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom PIMacs modules are enabled and what
;; order they load in.

(doom!
 :pimacs
 default ;; PIMacs default Emacs configuration on top of Doom.
 functions ;; Useful functions. Must be loaded for the keys module
 aliases ;; Define aliases starting with `_`. The most useful are `_rb` and `_sir`.
 theme ;; Define some faces and load Zenburn Theme. See the `README.md` in the directory `pimacs/theme`
 keys ;; Define the PIMacs keys binding. See the `refcard.md`.
 session ;; Auto reload the last session at startup through `doom/quickload-session` (no desktop bad practice in Doom)

 calendar-fr

 lang-c
 lang-lisp
 lang-make
 )

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/init.el")
;; End:
