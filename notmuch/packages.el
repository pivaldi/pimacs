;; -*- no-byte-compile: t; -*-
;;; pi/notmuch/packages.el

(if (file-directory-p "/home/pi/code/pi/emacs/notmuch-multi/")
    (package! notmuch-multi
      :recipe (:local-repo "/home/pi/code/pi/emacs/notmuch-multi/"
               :files ("*.el")))
  (package! notmuch-multi
    :recipe (:host github :repo "pivaldi/notmuch-multi")))

;; packages.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/notmuch/packages.el")
;; End:
