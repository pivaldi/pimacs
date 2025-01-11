;; -*- no-byte-compile: t; -*-
;;; pi/notmuch/packages.el

(package! notmuch-multi
  :recipe (:local-repo "/home/pi/code/pi/emacs/notmuch-multi/"
           :files ("*.el")))

;; (package! notmuch
;;   :recipe (:pre-build
;;            (with-temp-file "emacs/notmuch-version.el"
;;              (insert-file-contents "emacs/notmuch-version.el.tmpl")
;;              (re-search-forward "%VERSION%")
;;              (replace-match
;;               (format "\"%s+%s~%.7s\""
;;                       (with-temp-buffer (insert-file-contents "version.txt")
;;                                         (string-trim (buffer-string)))
;;                       (save-match-data
;;                         (let ((desc (doom-call-process "git" "describe" "--abbrev=7" "--match" "[0-9.]*")))
;;                           (if (zerop (car desc))
;;                               (car (last (split-string (cdr desc) "-") 2))
;;                             "??")))
;;                       (cdr (doom-call-process "git" "rev-parse" "HEAD")))
;;               t t)))
;;   :pin "969b26704da11c9e1935e5b442f6a88b32bb7e22")

;; packages.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/notmuch/packages.el")
;; End:
