;;; pimacs/notmuch/+notmuch.el -*- lexical-binding: t; -*-

;;; pimacs/notmuch/config.el -*- lexical-binding: t; -*-
;; Copyright (c) 2024, Philippe Ivaldi <www.piprime.fr>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Most code comes from
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-notmuch.el?ref_type=heads
;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el

;;; Code:

(defgroup pimacs-notmuch ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom pimacs-notmuch-delete-tag "delete"
  "Single tag that applies to mail marked for deletion.
This is used by `pimacs-notmuch-delete-mail'."
  :type 'string
  :group 'pimacs-notmuch)

(defcustom pimacs-notmuch-expire-tag "expire"
  "Single tag that applies to mail marked for expiration.
A message tagged as expirable will be removed after `'
This is used by `pimacs-notmuch-expire-mail'."
  :type 'string
  :group 'pimacs-notmuch)

(defcustom pimacs-notmuch-expire-delay 90
  "The age in days after which messages marked as expirable will be deleted.
See `pimacs-notmuch-expire-tag' and `pimacs-notmuch-expire-mail'."
  :type 'integer
  :group 'pimacs-notmuch)


(defcustom pimacs-notmuch-mark-delete-tags
  `(,(format "+%s" pimacs-notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion.
To actually delete email, refer to `pimacs-notmuch-delete-mail'."
  :type '(repeat string)
  :group 'pimacs-notmuch)

(defcustom pimacs-notmuch-mark-expire-tags
  `(,(format "+%s" pimacs-notmuch-expire-tag) "-inbox" "-unread")
  "List of tags to mark for expiration.
To delete expired emails, refer to `pimacs-notmuch-expire-mail'."
  :type '(repeat string)
  :group 'pimacs-notmuch)

(defcustom pimacs-notmuch-mark-flag-tags
  `(
    "+flagged" "-unread" "-spam"
    ,(format "-%s" pimacs-notmuch-delete-tag)
    ,(format "-%s" pimacs-notmuch-expire-tag)
    )
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'pimacs-notmuch)

(defcustom pimacs-notmuch-mark-spam-tags '("+spam" "-inbox" "-unread" "-archive")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'pimacs-notmuch)

(define-widget 'pimacs-notmuch-account-plist 'list
  "An email account.

An account is a plist. Supported properties are

  :name         The name of the account (required).
  :query        Search for all the email for this account (required).
  :key          Optional prefix shortcut key open `notmuch-jump-search' relatively to this account.
"
  :tag "Account Definition"
  :args '((list :inline t
           :format "%v"
           (group :format "%v" :inline t
                  (const :format "  Name: " :name)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Query: " :query)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Shortcut key prefix: " :key-prefix)
                  (string :format "%v")))
          ))

(define-widget 'pimacs-notmuch-accounts-saved-searches-plist 'list
  "A set of accounts associated with his saved searches list."
  :tag "Associated Account Searches"
  :args '((list :inline t
           :format "%v"
           (group :format "%v" :inline t
                  (const :format "" :account)
                  pimacs-notmuch-account-plist)
           (group :format "%v" :inline t
                  (const :format "Account Searches:\n" :searches)
                  (repeat :tag "Search" notmuch-saved-search-plist))
           )
          ))

(defcustom pimacs-notmuch-accounts-saved-searches
  `((:account (:name "MAIN" :query "*" :key ,(kbd "m"))
     :searches ,notmuch-saved-searches))
  "A list of email account associated with `notmuch-saved-searches'.

The saved accounts searches is a list of plist.
Supported properties of the plist are :

  :account         A `pimacs-notmuch-account-plist (required)'.
  :searches        A `notmuch-saved-searches' (required).
"
  :type '(repeat :tag "Account" pimacs-notmuch-accounts-saved-searches-plist)
  :tag "List of Accounts"
  :set (lambda (symbol value)
         (set-default symbol value)
         (pimacs-notmuch-accounts-saved-searches-set value))
  :group 'pimacs-notmuch)


(defun pimacs-notmuch-hello-filtered-query (query filter)
  "Constructs a query to search all messages matching QUERY and FILTER.
Modified version of `notmuch-hello-filtered-query' to handle query equal to *."
  (cond
   ((functionp filter) (funcall filter query))
   ((stringp filter)
    (if (string= query "*") filter
      (concat "(" query ") and (" filter ")")))
   (t query)))

(defun pimacs-notmuch-accounts-saved-searches-set (accounts-searches)
  "Setter of `pimacs-notmuch-accounts-saved-searches'.
Push the searches from accounts into `notmuch-saved-searches'
with computed nane, key and query.

Must be used instead of setq."
  (setq pimacs-notmuch-accounts-saved-searches accounts-searches)
  (dolist (account-searches accounts-searches)
    (let* ((searches (plist-get account-searches :searches))
           (account (plist-get account-searches :account))
           (account-name (plist-get account :name))
           (account-query (plist-get account :query))
           (kprefix (plist-get account :key-prefix)))
      (dolist (search searches)
        (let ((s (copy-sequence search)))
          (when search
            (when (not (string= kprefix ""))
              (let ((keydesc (key-description (plist-get search :key))))
                (plist-put s :key (kbd (concat kprefix keydesc))))
              ))
          (plist-put s :name (concat account-name " - " (plist-get search :name)))
          (plist-put s :query (pimacs-notmuch-hello-filtered-query account-query (plist-get search :query)))
          (add-to-list 'notmuch-saved-searches s)))
      )) notmuch-saved-searches)

(defface pimacs-notmuch-hello-header-face
  '((t :foreground "white"
     :background "blue"
     :weight bold))
  "Font for the header in `pimacs-notmuch-hello-insert-searches`."
  :group 'notmuch-faces)

(defface pimacs-notmuch-hello-buttons-unread-face
  '((t
     :inherit notmuch-tag-unread
     :weight bold))
  "Face used for unread hello buttons creation.
See `pimacs-notmuch-hello-insert-buttons`."
  :group 'notmuch-faces)

(defun pimacs--notmuch-remove-untags (tags)
  (let ((rmtags '()))
    (mapc
     (lambda (str)
       (when (string-match "^[^-]" str)
         (add-to-list 'rmtags str)
         )) tags)
    rmtags
    ))

(defmacro pimacs-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS.
Modified version of
https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags.

This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (let ((rmtags (pimacs--notmuch-remove-untags ,tags)))
       (when rmtags
         (notmuch-search-tag (notmuch-tag-change-list rmtags untag) beg end)
         (when (eq beg end)
           (notmuch-search-next-thread))))))

(pimacs-notmuch-search-tag-thread
  pimacs-notmuch-search-delete-thread
  pimacs-notmuch-mark-delete-tags)

(pimacs-notmuch-search-tag-thread
  pimacs-notmuch-search-expire-thread
  pimacs-notmuch-mark-expire-tags)

(pimacs-notmuch-search-tag-thread
  pimacs-notmuch-search-flag-thread
  pimacs-notmuch-mark-flag-tags)

(pimacs-notmuch-search-tag-thread
  pimacs-notmuch-search-spam-thread
  pimacs-notmuch-mark-spam-tags)

(defmacro pimacs-notmuch-search-tag-all (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Mark with `%s' all the messages in the search buffer.

Operate on each message in the current search buffer.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags."
       tags)
     (interactive current-prefix-arg)
     (let ((rmtags (pimacs--notmuch-remove-untags ,tags)))
       (when rmtags
         (notmuch-search-tag-all (notmuch-tag-change-list rmtags untag))))))


(pimacs-notmuch-search-tag-all
  pimacs-notmuch-search-delete-all
  pimacs-notmuch-mark-delete-tags)

(pimacs-notmuch-search-tag-all
  pimacs-notmuch-search-expire-all
  pimacs-notmuch-mark-expire-tags)

(pimacs-notmuch-search-tag-all
  pimacs-notmuch-search-flag-all
  pimacs-notmuch-mark-flag-tags)

(pimacs-notmuch-search-tag-all
  pimacs-notmuch-search-spam-all
  pimacs-notmuch-mark-spam-tags)

(defmacro pimacs-notmuch-tree-tag-message (name tags)
  "Produce NAME function parsing TAGS.
Modified version of
https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected message in notmuch-tree-mode.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags.

This function advances to the next message when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (let ((rmtags (pimacs--notmuch-remove-untags ,tags)))
       (when rmtags
         (notmuch-tree-tag (notmuch-tag-change-list rmtags untag))
         (notmuch-tree-next-message)))))

(pimacs-notmuch-tree-tag-message
  pimacs-notmuch-tree-delete-message
  pimacs-notmuch-mark-delete-tags)

(pimacs-notmuch-tree-tag-message
  pimacs-notmuch-tree-expire-message
  pimacs-notmuch-mark-expire-tags)

(pimacs-notmuch-tree-tag-message
  pimacs-notmuch-tree-flag-message
  pimacs-notmuch-mark-flag-tags)

(pimacs-notmuch-tree-tag-message
  pimacs-notmuch-tree-spam-message
  pimacs-notmuch-mark-spam-tags)


(defmacro pimacs-notmuch-tree-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' all message of the the current thread in notmuch-tree-mode.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (let ((rmtags (pimacs--notmuch-remove-untags ,tags)))
       (when rmtags
         (notmuch-tree-tag-thread (notmuch-tag-change-list rmtags untag))
         (notmuch-tree-next-thread)))))

(pimacs-notmuch-tree-tag-thread
  pimacs-notmuch-tree-delete-thread
  pimacs-notmuch-mark-delete-tags)

(pimacs-notmuch-tree-tag-thread
  pimacs-notmuch-tree-expire-thread
  pimacs-notmuch-mark-expire-tags)

(pimacs-notmuch-tree-tag-thread
  pimacs-notmuch-tree-flag-thread
  pimacs-notmuch-mark-flag-tags)

(pimacs-notmuch-tree-tag-thread
  pimacs-notmuch-tree-spam-thread
  pimacs-notmuch-mark-spam-tags)

(defmacro pimacs-notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS.
Source : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
              (notmuch-tag-change-list ,tags untag)))))

(pimacs-notmuch-show-tag-message
  pimacs-notmuch-show-delete-message
  pimacs-notmuch-mark-delete-tags)

(pimacs-notmuch-show-tag-message
  pimacs-notmuch-show-expire-message
  pimacs-notmuch-mark-expire-tags)

(pimacs-notmuch-show-tag-message
  pimacs-notmuch-show-flag-message
  pimacs-notmuch-mark-flag-tags)

(pimacs-notmuch-show-tag-message
  pimacs-notmuch-show-spam-message
  pimacs-notmuch-mark-spam-tags)


(defun pimacs-notmuch-hello-query-insert (cnt query elem)
  "Create a notmuch query widget.
Source : https://holgerschurig.github.io/en/emacs-notmuch-hello/"
  (if cnt
      (let* ((str (format "%s" cnt))
             (widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (oldest-first (cl-case (plist-get elem :sort-order)
                             (newest-first nil)
                             (oldest-first t)
                             (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                       :notify #'notmuch-hello-widget-search
                       :notmuch-search-terms query
                       :notmuch-search-oldest-first oldest-first
                       :notmuch-search-type 'tree
                       str)
        (widget-insert (make-string (- 8 (length str)) ? )))
    (widget-insert "        ")))

(defun pimacs-notmuch-hello-query-counts (query-list &rest options)
  "Modified version of `notmuch-hello-query-counts' to add unread the property :unread:count."
  (with-temp-buffer
    (dolist (elem query-list nil)
      (let* ((count-query (or (notmuch-saved-search-get elem :count-query)
                              (notmuch-saved-search-get elem :query)))
             (consolidated-count-query (replace-regexp-in-string
                                        "\n" " "
                                        (pimacs-notmuch-hello-filtered-query count-query
                                                                             (or (plist-get options :filter-count)
                                                                                 (plist-get options :filter))))))
        (insert
         consolidated-count-query "\n"
         (pimacs-notmuch-hello-filtered-query consolidated-count-query "tag:unread") "\n")))

    (unless (= (notmuch--call-process-region (point-min) (point-max) notmuch-command
                                             t t nil "count"
                                             (if (plist-get options :disable-excludes)
                                                 "--exclude=false"
                                               "--exclude=true")
                                             "--batch") 0)
      (notmuch-logged-error
       "notmuch count --batch failed"
       "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))
    (goto-char (point-min))
    (cl-mapcan
     (lambda (elem)
       (let* ((elem-plist (notmuch-hello-saved-search-to-plist elem))
              (search-query (plist-get elem-plist :query))
              (filtered-query (pimacs-notmuch-hello-filtered-query
                               search-query (plist-get options :filter)))
              (message-count (prog1 (read (current-buffer))
                               (forward-line 1)))
              (unread-count (prog1 (read (current-buffer))
                              (forward-line 1))))
         (when (and filtered-query (or (plist-get options :show-empty-searches)
                                       (> message-count 0)))
           (setq elem-plist (plist-put elem-plist :query filtered-query))
           (list (plist-put (plist-put elem-plist :count message-count) :unread-count unread-count)))))
     query-list)))

(defun pimacs-notmuch-hello-insert-buttons (searches)
  "Modified version of `notmuch-hello-insert-buttons'.

SEARCHES must be a list of plists each of which should contain at
least the properties :name NAME :query QUERY and :count COUNT,
where QUERY is the query to start when the button for the
corresponding entry is activated, and COUNT should be the number
of messages matching the query.  Such a plist can be computed
with `pimacs-notmuch-hello-query-counts'."
  (let* ((widest (notmuch-hello-longest-label searches))
         (tags-and-width (notmuch-hello-tags-per-line widest))
         (tags-per-line (car tags-and-width))
         (column-width (cdr tags-and-width))
         (column-indent 0)
         (count 0)
         (reordered-list (notmuch-hello-reflect searches tags-per-line))
         ;; Hack the display of the buttons used.
         (widget-push-button-prefix "")
         (widget-push-button-suffix ""))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
            ;; (not elem) indicates an empty slot in the matrix.
            (when elem
              (when (> column-indent 0)
                (widget-insert (make-string column-indent ? )))
              (let* ((name (plist-get elem :name))
                     (query (plist-get elem :query))
                     (oldest-first (cl-case (plist-get elem :sort-order)
                                     (newest-first nil)
                                     (oldest-first t)
                                     (otherwise notmuch-search-oldest-first)))
                     (search-type (plist-get elem :search-type))
                     (msg-count (plist-get elem :count))
                     (unread-count (plist-get elem :unread-count))
                     (title (if (eq 0 unread-count) name
                              (propertize name 'face 'pimacs-notmuch-hello-buttons-unread-face))))
                (widget-insert (format "%8s/%s "
                                       (if (eq 0 unread-count) (notmuch-hello-nice-number unread-count)
                                         (propertize
                                          (notmuch-hello-nice-number unread-count)
                                          'face 'pimacs-notmuch-hello-buttons-unread-face))
                                       (notmuch-hello-nice-number msg-count)))
                (widget-create 'push-button
                               :notify #'notmuch-hello-widget-search
                               :notmuch-search-terms query
                               :notmuch-search-oldest-first oldest-first
                               :notmuch-search-type search-type
                               title)
                (setq column-indent
                      (1+ (max 0 (- column-width (length name)))))))
            (cl-incf count)
            (when (eq (% count tags-per-line) 0)
              (setq column-indent 0)
              (widget-insert "\n")))
          reordered-list)
    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (unless (eq (% count tags-per-line) 0)
      (widget-insert "\n"))))

(defun pimacs-notmuch-hello-insert-searches (title query-list &rest options)
  "Insert a section with TITLE showing a list of buttons made from
QUERY-LIST.

QUERY-LIST should ideally be a plist but for backwards
compatibility other forms are also accepted (see
`notmuch-saved-searches' for details).  The plist should
contain keys :name and :query; if :count-query is also present
then it specifies an alternate query to be used to generate the
count for the associated search.

Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as
   its argument and returns a filter to be used in conjunction
   with the query for that search or nil to hide the
   element. This can also be a string that is used as a combined
   with each query using \"and\".
:filter-count - Separate filter to generate the count displayed
   each search. Accepts the same values as :filter. If :filter
   and :filter-count are specified, this will be used instead of
   :filter, not in conjunction with it."

  ;; (widget-insert "       ")
  (when (and notmuch-hello-first-run (plist-get options :initially-hidden))
    (add-to-list 'notmuch-hello-hidden-sections title))
  (let ((is-hidden (member title notmuch-hello-hidden-sections)))
    (if is-hidden
        (widget-create 'push-button
                       :notify (lambda (&rest _ignore)
                                 (setq notmuch-hello-hidden-sections
                                       (delete title notmuch-hello-hidden-sections))
                                 (notmuch-hello-update))
                       (concat title "..."))
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (add-to-list 'notmuch-hello-hidden-sections
                                            title)
                               (notmuch-hello-update))
                     title))
    (widget-insert "\n")
    (unless is-hidden
      (let ((searches (apply 'pimacs-notmuch-hello-query-counts query-list options)))
        (when (or (not (plist-get options :hide-if-empty))
                  searches)
          (widget-insert "\n")
          (pimacs-notmuch-hello-insert-buttons searches))))))


(defun pimacs-notmuch-hello-insert-account-searches (account-searches)
  "Insert a section of account associated with saved-searches.

See `pimacs-notmuch-accounts-saved-searches'."
  (let* ((searches (plist-get account-searches :searches))
         (account (plist-get account-searches :account))
         (account-query (plist-get account :query)))
    (pimacs-notmuch-hello-insert-searches
     (plist-get account :name) searches :filter account-query :show-empty-searches t)))

(defun pimacs-notmuch-hello-insert-accounts-searches ()
  (dolist (account-searches pimacs-notmuch-accounts-saved-searches)
    (when account-searches
      (pimacs-notmuch-hello-insert-account-searches account-searches)
      (widget-insert "\n")
      )))
