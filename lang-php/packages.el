;; -*- no-byte-compile: t; -*-
;;; pimacs/lang-php/packages.el

(package! php-cs-fixer)

(package! php-fh
  :recipe (:host github :repo "pivaldi/php-fh")
  :pin "261bb5499d7dac7ac5dee43034ad8a36411e9089"
  )

(provide 'pimacs/lang-php)
;;; packages.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/packages.el")
;; End:
