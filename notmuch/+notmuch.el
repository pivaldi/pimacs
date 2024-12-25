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
  "A single saved search property list."
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
                  (const :format "  Shortcut key: " :key)
                  (key-sequence :format "%v")))
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
  nil
  "plop."
  :type '(repeat :tag "Account" pimacs-notmuch-accounts-saved-searches-plist)
  :tag "List of Accounts"
  :group 'pimacs-notmuch)


;; (defcustom pimacs-notmuch-accounts-saved-searches
;;   nil
;;   ;; `(((:name "ivaldi.me" :query "tag:ivaldi.me" :key ,(kbd "i"))
;;   ;;   ((:name "Inbox" :query "tag:inbox" :key ,(kbd "i") :sort-order newest-first
;;   ;;     :search-type tree)
;;   ;;    (:name "Sent" :query "tag:sent" :key ,(kbd "s") :sort-order newest-first
;;   ;;     :search-type tree)))
;;   ;;   ((:name "ovya.fr" :query "tag:ovya.fr" :key ,(kbd "o"))
;;   ;;    ((:name "Inbox" :query "tag:inbox and not tag:redmine and not tag:admin"
;;   ;;      :key ,(kbd "i") :sort-order newest-first :search-type tree)
;;   ;;     (:name "Redmine" :query "tag:redmine" :key ,(kbd "r") :sort-order newest-first))))
;;   "A list of saved searches to display.

;; The saved search can be given in 3 forms. The preferred way is as
;; a plist. Supported properties are

;;   :name            Name of the search (required).
;;   :query           Search to run (required).
;;   :key             Optional shortcut key for `notmuch-jump-search'.
;;   :count-query     Optional extra query to generate the count
;;                    shown. If not present then the :query property
;;                    is used.
;;   :sort-order      Specify the sort order to be used for the search.
;;                    Possible values are `oldest-first', `newest-first'
;;                    or nil. Nil means use the default sort order.
;;   :search-type     Specify whether to run the search in search-mode,
;;                    tree mode or unthreaded mode. Set to `tree' to
;;                    specify tree mode, \\='unthreaded to specify
;;                    unthreaded mode, and set to nil (or anything
;;                    except tree and unthreaded) to specify search
;;                    mode.

;; Other accepted forms are a cons cell of the form (NAME . QUERY)
;; or a list of the form (NAME QUERY COUNT-QUERY)."
;;   ;; The saved-search format is also used by the all-tags notmuch-hello
;;   ;; section. This section generates its own saved-search list in one of
;;   ;; the latter two forms.
;;   ;; :get 'notmuch-hello--saved-searches-to-plist
;;   ;; :type '(repeat notmuch-saved-search-plist)
;;   :type '(repeat
;;           :tag "Accounts"
;;           (cons :tag "Account"
;;                 (pimacs-notmuch-account-plist)
;;                 (repeat :get 'notmuch-hello--saved-searches-to-plist
;;                         :tag "List of Saved Searches"
;;                         notmuch-saved-search-plist)))
;;   :tag "List of Associaded Saved Searches"
;;   :group 'pimacs-notmuch)

(defface pimacs-notmuch-hello-header-face
  '((t :foreground "white"
     :background "blue"
     :weight bold))
  "Font for the header in `pimacs-notmuch-hello-insert-searches`."
  :group 'notmuch-faces)

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
     (let ((rmtags '()))
       (mapc
        (lambda (str)
          (when (string-match "^[^-]" str)
            (add-to-list 'rmtags str)
            )) ,tags)

       (when rmtags
         (notmuch-search-tag
          (notmuch-tag-change-list rmtags untag) beg end)
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
             (oldest-first (case (plist-get elem :sort-order)
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


(defun pimacs-notmuch-hello-insert-searches ()
  "Insert the saved-searches section.
Source : https://holgerschurig.github.io/en/emacs-notmuch-hello/"
  (widget-insert (propertize "Ivaldi.me" 'face 'pimacs-notmuch-hello-header-face))
  (widget-insert (propertize "Ivaldi.m\nNew     Total      Key  List\n" 'face 'pimacs-notmuch-hello-header-face))
  (mapc (lambda (elem)
          (when elem
            (let* ((qtot (plist-get elem :query))
                   (qnew (concat qtot " AND tag:unread"))
                   (ntot (pimacs-notmuch-count-query qtot))
                   (nnew (pimacs-notmuch-count-query qnew)))
              (pimacs-notmuch-hello-query-insert nnew qnew elem)
              (pimacs-notmuch-hello-query-insert ntot qtot elem)
              (widget-insert "   ")
              (widget-insert (plist-get elem :key))
              (widget-insert "    ")
              (widget-insert (plist-get elem :name))
              (widget-insert "\n")
              ))
          )
        notmuch-saved-searches))
