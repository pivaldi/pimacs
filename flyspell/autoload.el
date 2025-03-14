;;; pimacs/flyspell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-ispell-dictionary-switch ()
  "Toggle dictionary between two dictionaries.

See `pim-flyspell-default-dictionary' and `pim-flyspell-secondary-dictionary'."
  (interactive)
  (if (string= ispell-current-dictionary pim-flyspell-default-dictionary)
      (ispell-change-dictionary pim-flyspell-secondary-dictionary)
    (ispell-change-dictionary pim-flyspell-default-dictionary)))

;;;###autoload
(defun pim-flyspell-correct ()
  "Use `flyspell-correct-at-point' if region is not active.
Use `ispell-region' if region is active."
  (interactive)
  (if  (region-active-p)
      (ispell-region (region-beginning) (region-end))
    (flyspell-correct-at-point)))
