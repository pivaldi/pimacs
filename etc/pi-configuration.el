;;; Package --- pi-configuration.el
;; Copyright (c) 2024, Philippe Ivaldi <www.piprim.net>

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

;;;; Commentary:

;; Set general parameters of Emacs for pimacs users

;;; Code:


(setq-default
 ;; Fix missing warning-suppress-types function
 warning-suppress-types nil
 ;; Draw block cursor as wide as the glyph under it
 x-stretch-cursor t
 ;; Title Format
 frame-title-format (list user-real-login-name "@" system-name " : %S" 'default-directory "%b")
 inhibit-startup-message t
 ;; Replace the bell by visible blink
 visible-bell t
 ;; I don't want graphic dialog.
 use-dialog-box nil
 use-file-dialog nil
 tab-width 4
 buffers-menu-max-size nil
 history-length 200
 scroll-step 1
 scroll-conservatively 10000
 ;; Increase the number of macro recursions
 ;; We can also set it to 9999 but then there may be stack overflows.
 max-lisp-eval-depth 5000
 max-specpdl-size max-lisp-eval-depth
 ;;permet d'ignorer la case dans le mode d'achévement
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;;when the mark is active, the region is highlighted.
 transient-mark-mode t
 ;; save everything before compiling
 compilation-ask-about-save nil
 ;; Size of the compilation window.
 compilation-window-height 12
 ;; Non-nil to scroll the *compilation* buffer window as output appears.
 compilation-scroll-output t
 ;; Allow local variables
 enable-local-variables t
 ;; Controls if scroll commands move point to keep its screen line unchanged.
 scroll-preserve-screen-position t
 ;; Number of lines of margin at the top and bottom of a window.
 ;; Pose des problèmes avec la version de gnus que j'utilise actuellement...
 ;; rajouter (set (make-local-variable 'scroll-margin 0)) dans le hook de `gnus-summary-mode-hook'
 scroll-margin 4
 ;; Maximum number of lines to keep in the message log buffer.
 message-log-max 1000
 ;; `apropos' search all
 apropos-do-all t ;; Key binding "C-h a".
 ;; Fill bulleted and indented lines
 adaptive-fill-mode t
 ;; Do not add a new string to `kill-ring' when it is the same as the last one.
 kill-do-not-save-duplicates t
 ;; split window preferred horizontally
 split-width-threshold most-positive-fixnum
 ;; Emacs will initiate GC every 20MB allocated because we have a modern machine
 gc-cons-threshold 20000000
 global-auto-revert-mode nil
 ;; Show all process with M-x proced
 ;; https://www.masteringemacs.org/article/displaying-interacting-processes-proced
 proced-filter 'all
 show-trailing-whitespace t
 show-leading-whitespace t
 nobreak-char-display t
 ;; Permanently force Emacs to indent with spaces, never with TABs:
 indent-tabs-mode nil
 ;; Maximum coloration
 font-lock-maximum-decoration t
 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
 display-line-numbers-type nil
 package-native-compile t
 )

;; ---------------------
;; * Prefered encoding *
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")

;; ----------------------------------------------------
;; * Auto-fill: coupure automatique de lignes longues *
;; ;; Pour ne pas que le mode auto-fill coupe à l'endroit d'un ":" ou ";" etc..
;; ;; Auteur: Matieux Moy http://www-verimag.imag.fr/~moy/emacs/
;; (defun pi-fill-nobreak-predicate ()
;;   (save-match-data
;;     (or (looking-at "[ \t]*[])}»!?;:]")
;;         (looking-at "[ \t]*\\.\\.\\.")
;;         (save-excursion
;;           (skip-chars-backward " \t")
;;           (backward-char 1)
;;           (looking-at "[([{«]")))))
;; (setq fill-nobreak-predicate (list 'pi-fill-nobreak-predicate))

(dolist (hook pi-auto-fill-mode-hook-alist)
  (add-hook hook
            (lambda ()
              (auto-fill-mode 1))))

;; -----------------------------------------------------
;; * Active le serveur Emacs pour utiliser emacsclient *
(when (string= system-type "gnu/linux")
  (require 'server)
  (unless server-process (server-start)))

;; -----------------------
;; * Remote File Editing *
;; I want to open a file remotely
(require 'tramp)
(setq tramp-default-method "ssh")

;; ;; -------------------------------------
;; ;; * Rendu HTML de buffers et fichiers *
;; ;; Just call `htmlize-view-buffer' to show the current buffer in
;; ;; your web browser.
;; (require 'htmlize-view)
;; (setq htmlize-convert-nonascii-to-entities nil)
;; (setq htmlize-html-charset "utf-8")
;; (htmlize-view-add-to-files-menu)

;; ;; ---------------------------------------
;; ;; * Affichage des paires de parenthèses *
;; ;; (GNU Emacs supports mic-paren only within a window-system but XEmacs
;; ;; supports mic-paren also without X)
;; (when (or (string-match "XEmacs\\|Lucid" emacs-version) window-system)
;;   (require 'mic-paren) ;; loading
;;   (paren-activate)     ;; activating
;;   (show-paren-mode 1)
;;   ;; Defines in which situations the whole sexp should be highlighted
;;   ;; You may customize `paren-face-match' with M-xcustomize-face
;;   (setq paren-sexp-mode (quote match))
;;   ;; set here any of the customizable variables of mic-paren:
;;   ;; Disable mic-paren in minibuffer (useful with the package ido)
;;   (add-hook 'minibuffer-setup-hook
;;             '(lambda ()
;;                (paren-deactivate)))
;;   (add-hook 'minibuffer-exit-hook
;;             '(lambda ()
;;                (paren-activate))))

;; ;; -------------
;; ;; * smart tab *
;; ;; Try to 'do the smart thing' when tab is pressed. `smart-tab';
;; ;; attempts to expand the text before the point or indent the current
;; ;; line or selection.
;; (when (require 'smart-tab nil t)
;;   (global-smart-tab-mode 1))

;; ;; -------------
;; ;; * Undo/Redo *
;; ;; C-/ pour undo C-: pour redo défini dans pi-keys.el
;; (require 'redo+)

;; ;; ----------------------------------------------
;; ;; * Indentation automatique dans certains mode *
;; ;; à  revoir en définissant une variable "liste de modes" et ajouter un hook sur les modes
;; (define-key prog-mode-map (kbd "RET") 'newline-and-indent)

;; ;; ----------------------------
;; ;; * Mettre en boite du texte *
;; ;; Commande M-x boxquote-...
;; ;; ,----
;; ;; | Voici du texte en boite
;; ;; `----
;; (require 'boxquote)

;; ;; --------------------------------------------------------
;; ;; * See https://www.emacswiki.org/emacs/GrepPlus#Grep%2b *
;; (require 'grep+)


;; (defun stop-using-minibuffer ()
;;   "Kill the minibuffer.
;; See https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html"
;;   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;     (abort-recursive-edit)))

;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ;; ---------------
;; ;; * Les ciseaux *
;; ;; Usage M-x separe <RET> donne
;; ;; 8<------8<------8<------8<------8<------8<------8<------8<------
;; (autoload (quote separe) "scissors" "Insert a line of SCISSORS in the buffer" t nil)

;; ;; Voir le nom de la fonction courante dans la ligne de mode
;; (which-function-mode 1)

;; ;; http://demo.icu-project.org/icu-bin/ubrowse?scr=55
;; ;; Symbols http://demo.icu-project.org/icu-bin/ubrowse?scr=55&b=0
;; ;; ☑

;; ;; http://www.fileformat.info/info/unicode/category/So/list.htm
;; (global-set-key (kbd "C-x 8") nil)
;; (global-set-key (kbd "C-x 8 1 / 3") "⅓")
;; (global-set-key (kbd "C-x 8 1 / 3") "⅓")
;; (global-set-key (kbd "C-x 8 1 / 5") "⅕")
;; (global-set-key (kbd "C-x 8 1 / 6") "⅙")
;; (global-set-key (kbd "C-x 8 2 / 3") "⅔")
;; (global-set-key (kbd "C-x 8 2 / 5") "⅖")
;; (global-set-key (kbd "C-x 8 3 / 5") "⅗")
;; (global-set-key (kbd "C-x 8 4 / 5") "⅘")
;; (global-set-key (kbd "C-x 8 5 / 6") "⅚")

;; (global-set-key (kbd "C-x 8 <") nil)
;; (global-set-key (kbd "C-x 8 . >") "→")
;; (global-set-key (kbd "C-x 8 . <") "←")
;; (global-set-key (kbd "C-x 8 > >") "↣")
;; (global-set-key (kbd "C-x 8 < <") "↢")
;; (global-set-key (kbd "C-x 8 < |") "↤")
;; (global-set-key (kbd "C-x 8 | >") "↦")
;; (global-set-key (kbd "C-x 8 < >") "↔")

;; (global-set-key (kbd "C-x 8 c") "♥")
;; (global-set-key (kbd "C-x 8 )") "☺")
;; (global-set-key (kbd "C-x 8 (") "☹")
;; (global-set-key (kbd "C-x 8 E") "★")
;; (global-set-key (kbd "C-x 8 e") "☆")

;; (global-set-key (kbd "C-x 8 z") "☤")
;; (global-set-key (kbd "C-x 8 v") "☣")
;; (global-set-key (kbd "C-x 8 r") "☢")
;; (global-set-key (kbd "C-x 8 !") "⚠")
;; (global-set-key (kbd "C-x 8 m") "☠")
;; (global-set-key (kbd "C-x 8 w") "☡")
;; (global-set-key (kbd "C-x 8 l") "⚡")
;; (global-set-key (kbd "C-x 8 RET") "⏎")

;; (global-set-key (kbd "C-x 8 f") "☭")
;; (global-set-key (kbd "C-x 8 p") "☮")
;; (global-set-key (kbd "C-x 8 y") "☯")
;; (global-set-key (kbd "C-x 8 u") "☝")
;; (global-set-key (kbd "C-x 8 s") "☘")
;; (global-set-key (kbd "C-x 8 b") "☕")
;; (global-set-key (kbd "C-x 8 k") "☑")

;; (global-set-key (kbd "C-x 8 T") "☎")
;; (global-set-key (kbd "C-x 8 t") "☏")

;; (global-set-key (kbd "C-h a") 'apropos)
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)

;; (defun next-user-buffer ()
;;   "Switch to the next user buffer.
;; User buffers are those whose name does not start with *."
;;   (interactive)
;;   (next-buffer)
;;   (let ((i 0))
;;     (while (and (string-match "^*" (buffer-name)) (< i 50))
;;       (setq i (1+ i)) (next-buffer) )))

;; (defun previous-user-buffer ()
;;   "Switch to the previous user buffer.
;; User buffers are those whose name does not start with *."
;;   (interactive)
;;   (previous-buffer)
;;   (let ((i 0))
;;     (while (and (string-match "^*" (buffer-name)) (< i 50))
;;       (setq i (1+ i)) (previous-buffer) )))

;; (global-set-key (kbd "<mouse-9>") 'next-user-buffer)
;; (global-set-key (kbd "<mouse-8>") 'previous-user-buffer)

;; (if (executable-find "rg")
;;     (grep-apply-setting
;;      'grep-find-command
;;      '("rg -n -H -i --hidden --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 39))
;;   (progn
;;     (add-to-list 'pi-error-msgs "Please install rg : https://github.com/BurntSushi/ripgrep")
;;     (grep-apply-setting
;;      'grep-find-command
;;      '("find . -type f ! -regex '.*/node_modules/.*' ! -regexp '.*/dist/.*' ! -regex '.*/vendor/.*' ! -regex '.*\\.git/.*' -exec grep -Hni '' {} \\;" . 132))))

(provide 'pi-configuration)
;; Local variables:
;; coding: utf-8
;; End:
