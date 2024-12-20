;;; pimacs/notmuch/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pimacs-notmuch-delete-mail ()
  "Permanently delete mail tagged with `pimacs-notmuch-delete-tag'.
Prompt for confirmation before carrying out the operation.

Do not attempt to refresh the index.  This will be done upon the
next invocation of 'notmuch new'."
  (interactive)
  (pimacs--notmuch-delete-mail-by-query (format "tag:%s" pimacs-notmuch-delete-tag)))

;;;###autoload
(defun pimacs-notmuch-delete-expirable-mail ()
  "Permanently delete expired email.

Expired mails are the ones marked as expirable and older
than `pimacs-notmuch-expire-delay'.
Prompt for confirmation before carrying out the operation.

Do not attempt to refresh the index.  This will be done upon the
next invocation of 'notmuch new'.

See `pimacs-notmuch-expire-tag'."
  (interactive)
  (pimacs--notmuch-delete-mail-by-query
   (format "tag:%s and date:@0..-%dd" pimacs-notmuch-delete-tag pimacs-notmuch-expire-delay)))

;;;###autoload
(defun pimacs-notmuch-delete-d+e-mail ()
  "Delete permanently the expired and the deletable emails.
See `pimacs-notmuch-delete-expirable-mail' and `pimacs-notmuch-delete-mail'."
  (interactive)
  (pimacs-notmuch-delete-mail)
  (pimacs-notmuch-delete-expirable-mail))

;;;###autoload
(defun pimacs-notmuch-count-query (query)
  "Simplified version of `notmuch-hello-query-counts'.
Usage : (pimacs-notmuch-count-query \"folder:here and tag:unread\")"
  (interactive "sQuery: ")
  (let ((count (string-to-number
                (with-temp-buffer
                  (shell-command
                   (format "notmuch count %s" query) t)
                  (buffer-substring-no-properties (point-min) (1- (point-max)))))))
    (message (format "%d" count))
    count))

(defun pimacs--notmuch-delete-mail-by-query (query)
  "Permanently delete mails matching the notmuch QUERY.
Prompt for confirmation before carrying out the operation.
Inspired from : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (interactive)
  (let* ((count (pimacs-notmuch-count-query query))
         (mail (if (> count 1) "mails" "mail")))
    (if (> count 0)
        (message "No mail matching `%s'" query)
      (when (yes-or-no-p
             (format "Delete permanently %d %s matching `%s' ?" count mail query))
        (shell-command
         (format "notmuch search --output=files --format=text0 %s | xargs -r0 rm" query)
         t)))))

;;;###autoload
(defun pimacs-notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'.
Source : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))
