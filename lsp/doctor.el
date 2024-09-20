;;; pimacs/lsp/doctor.el -*- lexical-binding: t; -*-

(unless (functionp 'json-serialize)
  (error!
   "Native JSON is *not* available. Please compile Emacs with the configure option --with-json"))
