;;; pimacs/pairing/doctor.el -*- lexical-binding: t; -*-

(unless (modulep! :config default +smartparens)
  (warn! "PIMacs pairing module is loaded but not Doom \":config default +smartparens\"."))
