;;; pimacs/org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pim-org-link-copy ()
  "Add to the kill ring the org-mode link at point."
  (interactive)
  (let* ((elt (org-element-context))
         (url (org-element-property :raw-link elt)))
    (if (org-element-type-p elt 'link)
        (progn
          (kill-new url)
          (message "Copied link: %s" url))
      (message "Not an Org Mode link…"))))
