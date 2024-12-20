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
  '(
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

(defmacro pimacs-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS.
Source : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.

This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

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
