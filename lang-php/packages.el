;; -*- no-byte-compile: t; -*-
;;; pimacs/lang-php/packages.el

(when (modulep! +php-cs-fixer)
  (package! php-cs-fixer))

(unless (modulep! +no-php-fh)
  (package! php-fh
    :recipe (:host github :repo "pivaldi/php-fh")
    :pin "63b9be69b529e471e58274ddaa3eebaf613473bd"))

(provide 'pimacs/lang-php/packages)
;;; packages.el ends here

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-php/packages.el")
;; End:
