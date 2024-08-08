(when (modulep! :editor evil)
  (warn! "PImacs does not provide good key configuration for evil mode. Push request is needed."))

(when (not (modulep! :pimacs functions))
  (error! "PIMacs functions module must be enabled to use the PIMacs keys module !"))

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/keys/doctor.el")
;; End:
