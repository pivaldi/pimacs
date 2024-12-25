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

;; Source of inspiration
;; https://protesilaos.com/emacs/dotemacs
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-notmuch.el?ref_type=heads
;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el

;;; Code:


(after! notmuch
  (load! "+notmuch")

  (setq
   ;; General UI
   notmuch-show-logo nil
   notmuch-column-control t
   notmuch-hello-auto-refresh t
   notmuch-hello-recent-searches-max 20
   notmuch-hello-thousands-separator " "
   notmuch-hello-sections '(
                            notmuch-hello-insert-saved-searches
                            ;; notmuch-hello-insert-search ;; I prefer the keybinding "s"
                            ;; notmuch-hello-insert-recent-searches ;; I prefer the keybinding "s" folowed by M-p
                            notmuch-hello-insert-alltags
                            ;; notmuch-hello-insert-footer ;; completly useless (hit the key "?")
                            pimacs-notmuch-hello-insert-searches
                            )
   notmuch-show-all-tags-list t

   ;; Set this to an arbitrary shell command
   +notmuch-sync-backend 'custom
   +notmuch-sync-command "~/bin/mail-sync.sh"

   ;; Encrypted and signed mime messages can be read and verified with
   notmuch-crypto-mime t
   notmuch-crypto-process-mime t
   mml-secure-openpgp-encrypt-to-self t
   )

  ;; Email composition settings
  (setq
   notmuch-always-prompt-for-sender t
   )


  ;; Saved search config
  (setq
   notmuch-show-empty-saved-searches t
   notmuch-saved-searches
   `(( :name "Inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "Unread+Inbox"
             :query "tag:unread and tag:inbox"
             :sort-order newest-first
             :key ,(kbd "u"))
     ( :name "Unread"
             :query "tag:unread"
             :sort-order newest-first
             :key ,(kbd "U"))
     ( :name "Archived"
             :query "tag:archive"
             :sort-order newest-first
             :key ,(kbd "a"))
     ( :name "Important"
             :query "tag:important"
             :sort-order newest-first
             :key ,(kbd "i"))
     ))

  ;; Tag settings
  (setq
   notmuch-archive-tags '("-inbox" "-unread" "+archived")
   notmuch-tag-formats
   '(("unread" (propertize tag 'face 'notmuch-tag-unread) "UR")
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged) "FL")
     ("delete" (notmuch-apply-face tag 'notmuch-tag-added) "DEL")
     ("expire" (notmuch-apply-face tag 'notmuch-tag-added) "EXP")
     ("important" (propertize tag 'face 'notmuch-tag-flagged) "❗"))
   notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-added) "R")
     (".*" (notmuch-apply-face tag 'notmuch-tag-deleted) tag))
   notmuch-tag-added-formats
   '(("delete" (notmuch-apply-face tag 'notmuch-tag-added) "DEL")
     ("expire" (notmuch-apply-face tag 'notmuch-tag-added) "EXP")
     (".*" (notmuch-apply-face tag 'notmuch-tag-added) tag)
     ))


;;;; Reading messages
  (setq
   ;; Inhibit displaying of images inline in the article body.
   mm-html-inhibit-images t
   ;; Which mime types to hide by default for multipart messages.
   notmuch-multipart/alternative-discouraged '("text/html" "multipart/related")
   ;; Display relative dates in the message summary line.
   notmuch-show-relative-dates t
   ;; Should all parts of multipart/alternative parts be shown ?
   notmuch-show-all-multipart/alternative-parts nil
   ;; Should the sub-parts of a multipart/* part be indented ?
   notmuch-show-indent-multipart nil
   ;; View the MIME part containing point, prompting for a viewer.
   notmuch-show-part-button-default-action 'notmuch-show-interactively-view-part
   ;; Remote images that have URLs matching this regexp will be blocked.
   notmuch-show-text/html-blocked-images "." ;; block everything
   notmuch-wash-wrap-lines-length 120
   ;; View selected messages in new window rather than split-pane.
   notmuch-unthreaded-show-out nil
   notmuch-message-headers '("To" "Cc" "Subject" "Date")
   notmuch-message-headers-visible t)

  (add-hook 'notmuch-mua-send-hook 'notmuch-mua-attachment-check)

  (map!
   :map global-map
   :desc "Run notmuch. #pim" "C-c m" #'notmuch
   :desc "Compose new mail with Notmuch. #pim" "C-x m" #'notmuch-mua-new-mail

   :map (notmuch-search-mode-map notmuch-tree-mode-map)
   :desc "Replace a by A. #pim" "a" nil ;; the default archive keybinding is too easy to hit accidentally
   :desc "Archive the currently selected thread or region. #pim" "A" #'notmuch-search-archive-thread
   :desc "Filter or LIMIT the current search results. #pim" "/" #'notmuch-search-filter ; alias for l
   :desc "Reply to the entire current thread. #pim" "r" #'notmuch-search-reply-to-thread-sender
   :desc "Reply-all to the entire current thread. #pim" "R" #'notmuch-search-reply-to-thread ; reply to all
   :desc "Mark as deleted the currently selected thread. #pim" "D" #'pimacs-notmuch-search-delete-thread
   :desc "Mark as expirable the currently selected thread. #pim" "E" #'pimacs-notmuch-search-expire-thread
   :desc "Mark as spam the currently selected thread. #pim" "S" #'pimacs-notmuch-search-spam-thread
   :desc "Refresh current buffer or all notmuch buffers if prefixed. #pim" "g" #'pimacs-notmuch-refresh-buffer

   :map notmuch-show-mode-map
   :desc "Archive the current message. #pim" "a" #'notmuch-show-archive-message
   :desc "Archive each message in thread. #pim" "A" #'notmuch-show-archive-thread
   :desc "Reply-to of the current message. #pim" "r" #'notmuch-show-reply-sender
   :desc "Reply-all of the current message. #pim" "R" #'notmuch-show-reply
   :desc "Tag as deleted or untag is prefixed. #pim" "D"  #'pimacs-notmuch-show-delete-message
   :desc "Tag as deleted or untag is prefixed. #pim" "E"  #'pimacs-notmuch-show-expire-message
   :desc "Tag as spam or untag is prefixed. #pim" "S" #'pimacs-notmuch-show-spam-message
   )

  ;; TODO
  ;; https://holgerschurig.github.io/en/emacs-notmuch-hello/ or https://sqrtminusone.xyz/posts/2021-02-27-gmail/
  ;; Integrate https://github.com/mhayashi1120/Emacs-langtool
  )

(provide 'pimacs/notmuch)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/notmuch/config.el")
;; End:
