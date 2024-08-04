(unless (modulep! +no-font)
  (when (not (find-font (font-spec :name "TerminessTTF NF")))
    (warn! "The Terminess TTF Nerd Fonts are not installed.")
    (explain! "Install the font from https://github.com/ryanoasis/nerd-fonts"
              "or load pimacs/theme with the flag `+no-font`\n")
    ))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/theme/doctor")
;; End:
