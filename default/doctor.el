(when (< emacs-major-version 28)
  (error! "Configuration not supported on Emacs < 28."))
(when (or (< emacs-major-version 29) (and (= emacs-major-version 29) (< emacs-minor-version 4)))
  (warn! "PIMacs is only tested with Emacs > 29.4."))
