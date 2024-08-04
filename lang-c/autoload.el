
;; Standardize indentation
;;;###autoload
(defun pim-c-indent-setup ()
  (setq-default c-basic-offset 2)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'brace-list-open '0)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'statement-case-open '0)
  (c-set-offset 'arglist-cont-nonempty '4)
  (c-set-offset 'arglist-intro 'c-basic-offset))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/lang-c/autoload.el")
;; End:
