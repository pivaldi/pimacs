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
  (use-package! notmuch-multi
    :defer nil
    :config
    ;; (load! "+notmuch")
    ;; (remove-hook! 'notmuch-show-hook '+notmuch-show-expand-only-unread-h)

    ;; General UI
    (setq
     notmuch-show-logo nil
     notmuch-column-control t
     notmuch-hello-auto-refresh t
     notmuch-hello-recent-searches-max 20
     notmuch-hello-thousands-separator " "
     notmuch-hello-sections '(
                              notmuch-multi-hello-insert-accounts-searches
                              ;; notmuch-hello-insert-saved-searches
                              ;; notmuch-hello-insert-search ;; I prefer the keybinding "s"
                              ;; notmuch-hello-insert-recent-searches ;; I prefer the keybinding "s" folowed by M-p
                              notmuch-hello-insert-alltags
                              ;; notmuch-hello-insert-footer ;; completly useless (hit the key "?")
                              )
     notmuch-show-all-tags-list t

     ;; Set this to an arbitrary shell command
     +notmuch-sync-backend "~/bin/mail-sync.sh"

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
    (setq notmuch-show-empty-saved-searches t)
    (set-face-attribute 'notmuch-tag-unread nil :inherit 'warning)
    (set-face-attribute 'notmuch-search-matching-authors nil :inherit 'notmuch-tree-match-author-face)
    (set-face-attribute 'notmuch-search-non-matching-authors nil :inherit 'notmuch-tree-match-author-face)
    (set-face-attribute 'notmuch-tree-no-match-author-face nil :inherit 'notmuch-tree-match-author-face)
    (set-face-attribute 'notmuch-tree-no-match-date-face nil :inherit 'notmuch-tree-match-date-face)
    (set-face-attribute 'notmuch-tree-no-match-tag-face nil :inherit 'notmuch-tree-match-tag-face)

    ;; Tag settings
    (setq
     notmuch-archive-tags '("-inbox" "-unread" "+archived")
     notmuch-tag-formats
     '(("unread" (propertize tag 'face 'notmuch-tag-unread) "U")
       ("flagged" (propertize tag 'face 'notmuch-tag-flagged) "🚩")
       ("inbox" (propertize tag 'face 'notmuch-tag-flagged) "I")
       ("delete" (notmuch-apply-face tag 'notmuch-tag-added) "D")
       ("archived" (notmuch-apply-face tag 'notmuch-tag-added) "A")
       ("sent" (notmuch-apply-face tag 'notmuch-tag-added) "S")
       ("expire" (notmuch-apply-face tag 'notmuch-tag-added) "E")
       ("attachment" (notmuch-apply-face tag 'notmuch-tag-added) "📎")
       ("important" (propertize tag 'face 'notmuch-tag-flagged) "❗")
       ("passed" (propertize tag 'face 'notmuch-tag-flagged) "P")
       ("replied" (propertize tag 'face 'notmuch-tag-flagged) "R")
       ("spam" (propertize tag 'face 'notmuch-tag-flagged) "🕱")
       ("signed" (propertize tag 'face 'notmuch-tag-flagged) "🔒"))
     )


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

     :map notmuch-hello-mode-map
     :desc "Move point to the previous field or button." "S-<tab>" #'widget-backward
     :desc "Move point to the previous field or button." "S-<iso-lefttab>" #'widget-backward

     :map notmuch-search-mode-map
     :desc "Replace a by A. #pim" "a" nil ;; the default archive keybinding is too easy to hit accidentally
     :desc "Archive the currently selected thread or region. #pim" "A" #'notmuch-search-archive-thread
     :desc "Filter or LIMIT the current search results. #pim" "/" #'notmuch-search-filter ; alias for l
     :desc "Reply to the entire current thread. #pim" "r" #'notmuch-search-reply-to-thread-sender
     :desc "Reply-all to the entire current thread. #pim" "R" #'notmuch-search-reply-to-thread ; reply to all
     :desc "Refresh current buffer or all notmuch buffers if prefixed. #pim" "g" #'notmuch-multi-refresh-buffer
     :desc "Mark as deleted the currently selected thread. #pim" "d" #'notmuch-multi-search-delete-thread
     :desc "Mark as deleted the currently selected thread. #pim" "D" #'notmuch-multi-search-delete-all
     :desc "Mark as expirable the currently selected thread. #pim" "e" #'notmuch-multi-search-expire-thread
     :desc "Mark as expirable the currently selected thread. #pim" "E" #'notmuch-multi-search-expire-all
     :desc "Mark as spam the currently selected thread. #pim" "s" #'notmuch-multi-search-spam-thread
     :desc "Mark as spam the currently selected thread. #pim" "S" #'notmuch-multi-search-spam-all
     :desc "Mark as flagged the currently selected thread. #pim" "!" #'notmuch-multi-search-flag-thread

     :map notmuch-tree-mode-map
     :desc "Replace a by A. #pim" "a" nil ;; the default archive keybinding is too easy to hit accidentally
     :desc "Archive the currently selected thread or region. #pim" "A" #'notmuch-tree-archive-thread
     :desc "Filter or LIMIT the current search results. #pim" "/" #'notmuch-tree-filter ; alias for l
     :desc "Reply to the sender of the current message. #pim" "r" #'notmuch-tree-reply-sender
     :desc "Reply-all of the current message. #pim" "R" #'notmuch-tree-reply ; reply to all
     :desc "Mark as deleted the currently selected message. #pim" "d" #'notmuch-multi-tree-delete-message
     :desc "Mark as deleted the currently selected thread. #pim" "D" #'notmuch-multi-tree-delete-thread
     :desc "Mark as expirable the currently selected message. #pim" "e" #'notmuch-multi-tree-expire-message
     :desc "Mark as expirable the currently selected thread. #pim" "E" #'notmuch-multi-tree-expire-thread
     :desc "Mark as spam the currently selected message. #pim" "s" #'notmuch-multi-tree-spam-message
     :desc "Mark as spam the currently selected thread. #pim" "S" #'notmuch-multi-tree-spam-thread
     :desc "Mark as flagged the currently selected thread. #pim" "F" #'notmuch-multi-tree-flag-thread
     :desc "Mark as flagged the currently selected message. #pim" "!" #'notmuch-multi-tree-flag-message
     :desc "Refresh current buffer or all notmuch buffers if prefixed. #pim" "g" #'notmuch-multi-refresh-buffer

     :map notmuch-show-mode-map
     :desc "Tag as flagged or untag is prefixed. #pim" "!" #'notmuch-multi-show-flag-message
     :desc "Archive the current message. #pim" "a" #'notmuch-show-archive-message
     :desc "Archive each message in thread. #pim" "A" #'notmuch-show-archive-thread
     :desc "Reply-to of the current message. #pim" "r" #'notmuch-show-reply-sender
     :desc "Reply-all of the current message. #pim" "R" #'notmuch-show-reply
     :desc "Tag as deleted or untag is prefixed. #pim" "C-t d"  #'notmuch-multi-show-delete-message
     :desc "Tag as deleted or untag is prefixed. #pim" "C-t e"  #'notmuch-multi-show-expire-message
     :desc "Tag as spam or untag is prefixed. #pim" "C-t s" #'notmuch-multi-show-spam-message
     )

    (setq notmuch-search-result-format
          '(("date" . "%12s ") ("count" . "%-7s ") ("authors" . "%-30s ")
            (notmuch-multi-search-format-subject . "%-90s ") ("tags" . "(%s)"))
          notmuch-tree-result-format
          '(("date" . "%12s  ") ("authors" . "%-30s")
            ((("tree" . "%s") (notmuch-multi-tree-format-subject . " %-80s")) . " %-90s ") ("tags" . "(%s)"))
          notmuch-unthreaded-result-format
          '(("date" . "%12s ") ("authors" . "%-30s ")
            (notmuch-multi-search-format-subject . "%-90s ") ("tags" . "(%s)")))

    (setq notmuch-thread-symbols
          '((prefix . " ")
            (top . "─")
            (top-tee . "┬")
            (vertical . "│")
            (vertical-tee . "├")
            (bottom . "└")
            ;; (arrow . "►")
            (arrow . "─►")
            ))

    ;; TODO
    ;; https://holgerschurig.github.io/en/emacs-notmuch-hello/ or https://sqrtminusone.xyz/posts/2021-02-27-gmail/
    ;; Integrate https://github.com/mhayashi1120/Emacs-langtool
    ))

(provide 'pimacs/notmuch)
;; config.el ends here.

;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/notmuch/config.el")
;; End:
