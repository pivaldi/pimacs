;; Copyright (c) 2011, Philippe Ivaldi <www.piprime.fr>
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation ; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; COMMENTARY:

;; THANKS:

;; BUGS:

;;; CODE:

;; Set the debug option to enable a backtrace when
;; problems occur.
(defvar pi-error-msgs (list)
  "* List of errors encountered when loading pi-configuration files.")

(when (< emacs-major-version 27)
  (setq inhibit-startup-message t)
  (error "Configuration not supported on Emacs < 27."))

;; *=======================================================*
;; *................Gestion des répertoires................*
;; *=======================================================*


(add-load-path! "pimacs/site-lisp")
(add-load-path! "pimacs/etc")

;; (load! "pi-custom-definition")

;; *=======================================================*
;; *.............chargement des configurations.............*
;; *=======================================================*

(load! "etc/pi-defcustom") ;; Early loading needed
(load! "etc/pi-doom") ;; Early loading needed

(load! "etc/pi-theme")
(load! "etc/pi-configuration")
(load! "etc/pi-aliases")
(load! "etc/pi-hooks")
(load! "etc/pi-modes")
(load! "etc/pi-desktop")

;; (straight-use-package '(etc :type built-in :local-repo "etc"))

(load! "etc/pi-functions")

(load! "etc/pi-keys")

(after! cc
        (load! "etc/pi-c"))

;; ;; ---------------------------------------
;; ;; * Project interaction library for Emacs *
;; ;; Open a file in one of your projects and type a command such as C-c p f
;; (load "pi-projectile")

;; ;; ---------------------------------------
;; ;; * Tree layout file explorer for Emacs *
;; (load "pi-treemacs")

;; ;; ------------------------------
;; ;; * on-the-fly syntax checking *
;; (load "pi-flymake")

;; ;; ----------------------------------------------------
;; ;; * Major mode for editing Lisp code to run in Emacs *
;; (load "pi-elisp")

;; ;; -----------------------
;; ;; * A emacs tree plugin *
;; ;; (load "pi-neotree") ;; Conflict with treemacs

;; ;; --------------------------------------------
;; ;; * Pieces of code that interressent only me *
;; (when (locate-library "pi-only")
;;   (load "pi-only"))

;; ;; ------------------
;; ;; * Special config *
;; ;; Things that are not usually necessary but;
;; ;; that I like (eg total absence of bars)
;; ;; Shortcuts defined:
;; ;; C-f1 to toggle the visibility of the menu bar
;; (when (string= user-real-login-name "pi")
;;   (load "pi-unwanted"))

;; ;; -----------------------
;; ;; * Dates et calendrier *
;; (load "pi-time")

;; ;; ---------
;; ;; * Tramp *
;; (load "pi-tramp")

;; ;; ----------------
;; ;; * Windows mode *
;; ;;; pi-windows  enables  you  to  have  multiple  favorite  window
;; ;;; configurations at the same  time, and switch them.  Furthermore,
;; ;;; it  can  save  all  window  configurations and  some  global  or
;; ;;; buffer-local variables into a file and restore them correctly.
;; ;;;       The default prefix key stroke for Windows is `C-c C-w'.  If it
;; ;;; causes  you some  troubles, see  the  section  `Customizations'.
;; ;;; Here are the default key bindings.
;; ;;;
;; ;;;  C-c C-w 1  Switch to window 1 (Q)
;; ;;;  C-c C-w 2  Switch to window 2 (Q)
;; ;;;
;; ;;;  C-c C-w 9  Switch to window 9 (Q)
;; ;;;  C-c C-w 0  Swap windows with the buffer 0 (Q)
;; ;;;     (Select unallocated frame(Emacs 19))
;; ;;;  C-c C-w SPC  Switch to window previously shown (Q)
;; ;;;  C-c C-w C-n  Switch to next window
;; ;;;  C-c C-w C-p  Switch to previous window
;; ;;;  C-c C-w !  Delete current window (Q)
;; ;;;  C-c C-w C-w  Window operation menu
;; ;;;  C-c C-w C-r  Resume menu
;; ;;;  C-c C-w C-l  Local resume menu
;; ;;;  C-c C-w C-s  Switch task
;; ;;;  C-c C-w =  Show window list (Q)
;; (load "pi-windows")

;; ;; -------------------------
;; ;; * InteractivelyDoThings *
;; ;; See https://www.emacswiki.org/emacs/InteractivelyDoThings
;; ;; Handle flx-ido package if installed
;; (load "pi-ido")

;; ;; ----------------
;; ;; * For SQL mode *
;; (load "pi-sql.el")

;; ;; -----------------------------------
;; ;; * Configuration de base pour Gnus *
;; (when (string= (user-real-login-name) "pi")
;;   (load "pi-gnus")) ;; n'est chargé que chez moi :-)

;; ;; -----------
;; ;; * Twitter *
;; ;; (load "pi-twit") ;; does not work

;; ;; --------------------------
;; ;; * Fonctions personnelles *
;; ;; Functions définies (utilisables par M-x function <ret>) :
;; ;; xpdf : voir le .pdf associé au buffer avec xpdf.
;; ;; acro : voir le .pdf associé au buffer avec acrobatreader.
;; ;; gv   : voir le .pdf associé au buffer avec gv.
;; ;; separe : insère une une ligne de séparation 8<------8<---etc...
;; ;; moy-insert-set-key : insère le code pour définir un raccourci global.
;; ;; pi-last-modified: ajoute la date courante (bindé sur f11)
;; ;; pi-increment-number-line : copie n lignes et incrémente les nombres trouvés
;; ;;                            usage C-u n M-x pi-increment-number-line
;; ;; pi-increment-number-decimal : incrémente le nombre sous le curseur de n (1 par défaut, sans C-u)
;; ;;  C-c + ou C-c - ou C-u n C-c + etc…
;; ;; pi-insert-date
;; ;; C-c d: 11.04.2018
;; ;; C-u C-c d: 2018-04-11
;; ;; C-u C-u C-c d: Wednesday, 11. April 2018

;; (require 'pi-functions)

;; ;; ----------------------------------
;; ;; * I want PI's keyboard shortcuts *
;; ;; Redéfini C-k pour que le résultat soit parfait en fin de ligne (voir C-h k C-k)
;; ;; C-z        : bascule plein écran
;; ;; F8         : affiche le nom du fichier courant
;; ;; C-u F8     : insère le nom du fichier courant dans le buffer
;; ;; S-f8       : affiche le nom du fichier courant et le place dans le kill ring
;; ;; C-next et C-prior : pour remplacer C-x o et C-u C-x o trop long pour moi ;-)
;; ;;;pour ceux qui ne connaisent pas ça permet de parcourir les frames dans le sens désiré:
;; ;;;essayer C-x 3 C-x 2 puis c-next 3 fois c-prior 3 fois. Terminer par C-x 1
;; ;; S-TAB      : complète un nom de fichier directement depuis le buffer courant, très utile !
;; ;;;essayer ~/ema et S-TAB juste après le 'a'
;; ;; C-x b      : visite le lien sous le curseur avec votre navigateur préféré.
;; ;; F12        : ferme le fichier courant ET la fenêtre (10 fois plus pratique rapide que C-x k C-x 0)
;; ;; C-S-TAB    : indente proprement TOUT le buffer sans changer la place du curseur et en respectant
;; ;;;les balises <pre></pre> du html !
;; ;; M-up ou M-down : transpose la ligne avec la précédente/suivante.
;; ;; C-x c-r    : permet d'ouvrir un fichier en tant que root.
;; ;; redéfini la touche 'home' pour qu'elle fonctionne comme tout le monde voudrait qu'elle le fasse ;-)
;; ;; M-q        : Use fill line or region as auto-fill-mode does
;; ;; C-%        : commente ou décommente la ligne courante ou la région et indente le code
;; ;; C-ù        : idem mais n'indente pas, utile en mode fondamental
;; ;; C-* et C-µ : pour insérer les commentaires spéciaux section (en jaune) et sous-section (en blanc)
;; ;;;dans du code Elisp
;; ;; F9         : cherche récursivement un makefile dans les répertoires supérieurs
;; ;;;en partant du répertoire courant
;; ;; Scroll_Lock ou Num-Défil : bascule en défilement de la page et mode normal.
;; ;; C-$ |  C-" | C-{  | C-( : dans certain mode $, {, (, [ exécutent `skeleton-pair-insert-maybe'
;; ;;;précédé de 'Control', cette fonctionnalité est ignorée.
;; ;; C-:        : redo (undo c'est C-/ par défaut)
;; ;; Redéfini les guillemets « et » pour ajouter les espaces insécables.
;; ;; f10        : bascule inverse-video la colonne courante
;; ;; C-<        : pour revenir à la position précédente
;; ;; C->        : pour revenir à la position suivante
;; ;; C-x up | C-x down | C-x right | C-x left pour redimensionner les fenêtres
;; ;;;essayer C-x 3 C-x 2 puis C-x une_flèche. S'amuser alors avec les flèches seules ; space pour finir
;; ;; C-' insère l'apostrophe en utf8 : ’
;; ;; F11        : met à jour l'information de dernière modification si elle existe
;; ;;;précédé de C-u, insère la date de dernière modification au tout début du fichier
;; ;; S-F4       : démarre l'enregistrement d'une macro
;; ;; F4         : termine l'enregistrement d'une macro s'il y en a une en cours d'enregistrement,
;; ;; C-;   : insert a semicolon at the end of the line if it does not exist
;; ;; C-,   : insert a comma at the end of the line if it does not exist
;; ;;;sinon exécute la dernier macro enregistrée
;; ;;;Exemple : ouvrir un fichier contenant du texte, se placer au début, presser S-F4,
;; ;;;descendre de 3 lignes, presser la touche « entrée », presser F4. La macro est enregistrée,
;; ;;;represser F4 pour exécuter cette macro.
;; (load "pi-keys")

;; ;; --------------
;; ;; * Organiseur *
;; (load "pi-org")

;; (when (locate-library "auctex")
;;   ;; ---------
;;   ;; * LaTeX *
;;   ;; Raccourcis (re)définis:
;;   (load "pi-tex")

;;   ;; ------------
;;   ;; * Metapost *
;;   (load "pi-meta")
;;   )

;; ;; ---------------------------------------------
;; ;; * Abbreviations et complétion avec tempo.el *
;; ;; La complétion avec tempo est utile mais sera, à plus ou moins
;; ;; long terme, remplacée par yasnippet (voir ci-après)
;; ;; Raccourcis définis:
;; ;; S-f4   : Démarre la définition d'une macro
;; ;; f4     : Termine la définition de marco en cours sinon execute la dernière macro.
;; ;; C-f4   : Édite la dernière macro
;; ;; f7     : Bascule du mode abbrev-mode

;; ;; f3      : complète le symbole tempo avant le curseur
;; ;; M-left  : revient à la marque précédente
;; ;; M-right : va à la marque suivante
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Voir dans le fichier pi-abbrev&tempo.el comment enregistrer une macro.
;; ;; Voir dans le fichier pi-abbrev&tempo.el comment définir et enregistrer des abréviations.
;; (load "pi-abbrev&tempo")

;; ;; ----------------------
;; ;; * Complétion avancée *
;; ;; Deux modes de completion automatique : auto-complete et company
;; ;; Raccourcis définis:
;; ;; M-space : initiate completion manually via company-complete
;; (load "pi-complete")

;; ;; ---------
;; ;; * gtags *
;; ;; Pour les programmeurs qui connaissent etag (sinon désactiver)
;; ;; Voir la documentation ici: http://www.gnu.org/software/global/globaldoc.html
;; ;; Raccourcis définis:
;; ;; M-.   : finds tag
;; ;; C-M-. : find all references of tag
;; ;; C-M-, : find all usages of symbol.
;; ;; M-;   : cycles to next result, after doing M-. C-M-. or C-M-,
;; ;; C-;   : insert a semicolon at the end of the line if it does not exist
;; (load "pi-gtags")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; -------------------------------
;; ;; * Les indispensables snippets *
;; ;; http://code.google.com/p/yasnippet/
;; ;; See https://github.com/hlissner/emacs-snippets for the list of available snippets
;; ;; plus the directory tree site-lisp/pi-snippets
;; (load "pi-snippets")


;; ;; -------
;; ;; * php *
;; ;; For the PHP programmers
;; ;; C-S-f8 : insert the php namespace clause based on first upper
;; ;;          case letter naming convention
;; (load "pi-php")

;; ;; -------
;; ;; * go *
;; ;; For the Go programmers
;; ;; C-c C-a : go-import-add will prompt you for an import path
;; ;; C-c C-r : go-remove-unused-imports detect all unused imports and delete them
;; ;; C-c i   : go-goto-imports
;; ;; C-M-a   : beginning-of-defun
;; ;; C-M-e   : end-of-defun
;; ;; C-M-h   : mark-defun
;; ;; C-c C-d : godef-describe
;; ;; M-.     : godef-jump (default is C-c C-j)
;; (load "pi-go")

;; ;; ---------------------------------------------------------
;; ;; * Emacs client/library for the Language Server Protocol *
;; (load "pi-lsp")

;; ;; --------
;; ;; * html *
;; ;; For editing html files
;; (load "pi-html")

;; ;; -------
;; ;; * xml *
;; ;; For editing xml files
;; (load "pi-xml")

;; ;; -----------
;; ;; * mmm-mode *
;; ;; - Configure multimode for emacs
;; ;;   here doc syntax toggle to XXX-mode with this syntax (php-mode here) :
;; ;;   $MY_VAR = <<<SQL
;; ;;    [SQL CODE]
;; ;;   SQL;
;; ;; - In HTML-MODE, the <?php ?> flag toggle to php-mode
;; ;; (load "pi-mmm-mode")

;; ;; -------
;; ;; * c++ *
;; (load "pi-cpp")
;; ;; Major mode for editing CTemplate files.
;; (load "pi-ctpl")
;; ;; Config for edit C++ with Qt
;; (load "pi-qt")

;; ;; ----------
;; ;; * python *
;; ;; For the Python programmer
;; ;; Define "C-c <down>" : py-donw
;; ;; and "C-c <up>" : py-up
;; ;; and force C-M-{up,down} to scroll-move-{up,down}
;; ;; Default anaconda key binding :
;; ;; C-M-i anaconda-mode-complete
;; ;; M-. anaconda-mode-find-definitions
;; ;; M-, anaconda-mode-find-assignments
;; ;; M-r anaconda-mode-find-references
;; ;; M-* anaconda-mode-go-back
;; ;; M-? anaconda-mode-show-doc
;; ;; (load "pi-python")

;; ;; ---------
;; ;; * CEDET *
;; ;; For advanced programming
;; ;; Must be load before ECB !
;; (load "pi-cedet")

;; ;; -------
;; ;; * ECB *
;; ;; For navigating in programming code
;; ;; (load "pi-ecb")

;; ;; -------
;; ;; * jde *
;; ;; For the Java programmer
;; ;; (load "pi-jde")

;; ;; --------------------------------
;; ;; * For debug programs *
;; (load "pi-geben") ;; See the documentation in the file pi-geben.el

;; ;; ----------
;; ;; * Jabber *
;; (load "pi-jabber")

;; ;; --------------
;; ;; * TypoScript *
;; ;; (load "pi-typoscript")

;; ;; ------------------------
;; ;; * Programmation en LUA *
;; (load "pi-lua")

;; ;; ----------------------------
;; ;; * Programmation en haskell *
;; ;; (load "pi-haskell") ;

;; ;; -------------------------
;; ;; * Muse for text to html *
;; (load "pi-muse")

;; ;; -----------------
;; ;; * Crypt buffers *
;; ;; Use crypt++ For Emacs <=22 and Epa for Emacs >22
;; ;; With Epa you must have a key http://emacs.wordpress.com/2008/07/18/keeping-your-secrets-secret/
;; ;; The files with the extension .gpg will be crypted !!
;; ;; Very useful to keep pass word and other confidential data
;; (load "pi-crypt")

;; ;; -----------------------
;; ;; * Editeur de tableaux *
;; ;; M-x table-insert pour créer un tableau
;; ;; M-x table-generate-source pour l'exporter en LaTeX, html...
;; ;; M-x ltxtab-format pour formater un tableau en LaTeX
;; ;; (load "pi-table")

;; ;; -----------------------------------------------------
;; ;; * Completion and cycling of completion candidates.  *
;; ;; Plus d'info: http://www.emacswiki.org/cgi-bin/wiki/Icicles
;; ;; Icicle est utile mais un peu trop envahissant pour l'activer par défaut
;; ;; icicles doit être appelé le plus tard possible
;; ;; comme j'utilise "desktop-save" j'ai choisi la méthode suivante:
;; ;; (load "pi-icicle") ;; un peu trop envahissent à mon goût...

;; ;; ---------------------------------
;; ;; * Le Menu depuis le clavier svp *
;; ;; <ESC> M-x. On vous demande le nom du menu à executer; commencer à taper son nom...
;; (load "pi-lacarte")

;; ;; ---------------------
;; ;; * Configuration FTP *
;; ;; À configurer suivant vos serveur
;; ;; ¡l y a en particulier les serveurs qui demandent un mode passif !!
;; (load "pi-ftp")

;; ;; --------
;; ;; * yaml *
;; ;; Pour éditer des fichier yml (sinon à virer sans crainte)
;; (load "pi-yaml")

;; ;; --------
;; ;; * sass *
;; ;; Pour éditer des fichier sass (sinon à virer sans crainte)
;; ;; http://sass-lang.com/ et le css devient merveilleux
;; (load "pi-sass")

;; ;; ------------------------
;; ;; * dired: File Explorer *
;; ;; A little hard to take over but no explorer
;; ;; file does it comes close.
;; ;; See file for more information
;; (load "pi-dired")

;; ;; -----------------------------------
;; ;; * Formal calculations with Maxima *
;; ;; (load "pi-maxima")

;; ;; ----------------------------------------------
;; ;; * Complex numerical calculations with Scilab *
;; ;; (load "pi-scilab")

;; ;; ------------------------------------------
;; ;; * Complex numerical calculations Pari/gp *
;; ;; (load "pi-pari-gp")

;; ;; -------------------------------
;; ;; * Synthetic image with PovRay *
;; ;; (load "pi-povray")

;; ;; ------------------------
;; ;; * Bongo : Audio Player *
;; ;; To listen music from Emacs, otherwise can be fired safely
;; (load "pi-bongo")

;; ;; -----------------------
;; ;; * Emms : Audio Player *
;; ;; Unable to get used to it
;; ;; (load "pi-emms")

;; ;; ------------------------------------
;; ;; * Visible, buffer local, bookmarks *
;; ;; Description:
;; ;;   bm.el provides visible, buffer local, bookmarks and the ability
;; ;;   to jump forward and backward to the next bookmark.
;; ;;   More informations in bm.el.
;; ;; Defined shortcuts:
;; ;;; f2   : Go to the next bookmark
;; ;;; C-f2 : Add/Remove a bookmark
;; ;;; S-f2 : Toggle if a buffer has persistent bookmarks or not.
;; (load "pi-bm")

;; ;; -----------------
;; ;; * markdown mode *
;; ;; Documentation here http://jblevins.org/projects/markdown-mode/
;; (load "pi-markdown")
;; ;;
;; ;; ----------------------------------------
;; ;; * Correction orthographique à la volée *
;; ;; Flyspell http://kaolin.unice.fr/~serrano
;; ;; Shortcuts defined :
;; ;;; f6 switch français/américain
;; ;;; M-$ to check the word at point
;; (load "pi-flyspell")

;; ;; -------------------
;; ;; * scroll-in-place *
;; ;; scroll-in-place is a package that keeps the cursor on the same line
;; ;; (and in the same column) when scrolling by a page using PgUp/PgDn.
;; ;; Shortcuts defined :
;; ;; C-up et C-down to scroll the text without changing the cursor position.
;; ;; C-M-up et C-M-down to scroll the page.
;; (load "pi-scroll")

;; ;; ------------------------------------------------
;; ;; * Permutation aléatoire de lettres ds un texte *
;; ;; Auteur: http://christophe.deleuze.free.fr/emacs.html
;; ;; Sélectionner un texte et M-x scramble-region <RET>
;; ;; C'est rigolo :-)
;; ;; (load "pi-scramble")

;; ;; -----------------------------------------
;; ;; * Conversion d'expressions parenthésées *
;; ;; Par exemple, sélectionner la ligne:
;; ;; (a + b * (c + d* (e + f))) - a * (c + d (e + f))
;; ;; M-x pi-exp-paren-region <RET> produit:
;; ;; \Big(a + b * \big(c + d* (e + f)\big)\Big) - a * \big(c + d (e + f)\big)
;; ;; Pour redéfinir des tags:
;; ;; (setq pi-mm-paren-list '("" "\\big" "\\Big" "\\bigg" "\\Bigg"))
;; ;; (load "pi-exp-paren")

;; ;; ----------------------------
;; ;; * Extended AutoText models *
;; ;; Auto-insert template in new file.
;; ;; The models are in the ~/emacs.d/site-lisp/template/ and can content embeded Lisp code
;; ;; Whatever is between the pairs !§! is interpreted by Emacs as Emacs Lisp code.
;; ;; The special tag !§!-!§! is the position of the cursor once the model is inserted.
;; ;; It is possible to have several models for a given mode, the models are defined by
;; ;;;their extensions (the .el for the lisp, the .html for the html etc)
;; ;;;except the file makefile which is the template for a makefile
;; ;; Also define `pi-template-licence' to insert a licence template (extension .licence)
;; (load "pi-template-conf")

;; ;; ------------------------------------------
;; ;; * Asymptote to create scientific figures *
;; ;; ForAsymptote : http://asymptote.sourceforge.net/
;; ;; Shortcuts defined :
;; ;; f1 : if etags is available, create the files TAGS for Asymptote
;; ;; C-c C-p : to force the viewing in pdf
;; ;; C-c C-a : to make an animation with beamer of Asymptotes code
;; ;;;(contact me for the script ...) that said, it's not very useful…
;; (load "pi-asy")

;; ;; ----------------------------
;; ;; * BBDB le carnet d'adresse *
;; (load "pi-bbdb")

;; ;; ----------------------------
;; ;; * Pour coder en Javascript *
;; ;; F1    : show all the functions or methods defined in the current buffer
;; (load "pi-js")
;; (load "pi-web-beautify")

;; ;; ---------------------
;; ;; * Coding Typescript *
;; ;; C-c d  Show documentation for the symbol at point.
;; ;; M-.    Jump to the definition of the symbol at point. With a prefix
;; ;;        arg, Jump to the type definition.
;; ;; M-,    Return to your pre-jump position.
;; ;; ¹ (available by S-²) is rebound to complete filename relatively to the src directory
;; ;; details here https://github.com/ananthakumaran/tide
;; (load "pi-typescript")

;; ;; ----------------------------------------------
;; ;; * Manage your `kill-ring' (select and paste) *
;; ;; Open a fancy buffer to show the kill-ring
;; ;; Key binding defined : C-c y to show the pop-up (use right arrow to show the content)
;; (load "pi-browse-kill-ring") ;; Not needed with helm


;; ;; --------------------------------------------------------------------------
;; ;; * Octave GNU : programming language intended for numerical computations. *
;; ;;(load "pi-octave")

;; ;; -----------------------------------------------------
;; ;; * Minor mode for visual feedback on some operations *
;; (load "pi-highlights")


;; ;; --------------------------------------
;; ;; * Minor mode for parenthesis editing *
;; (load "pi-paren")

;; ;; (setq minibuffer-max-depth nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

;; ;; IMPORTANT NOTE :
;; ;; see helm usage here : http://tuhdo.github.io/helm-intro.html
;; ;; and http://tuhdo.github.io/helm-projectile.html
;; ;; --------------------
;; ;; * Enable helm-mode *
;; ;; C-c p h   helm-projectile All-in-one command
;; ;; C-c p h   helm-projectile Helm interface to projectile
;; ;; C-c p p   helm-projectile-switch-project Switches to another projectile project
;; ;; C-c p f   helm-projectile-find-file Lists all files in a project
;; ;; C-c p F   helm-projectile-find-file-in-known-projects Find file in all known projects
;; ;; C-c p g   helm-projectile-find-file-dwim Find file based on context at point
;; ;; C-c p d   helm-projectile-find-dir Lists available directories in current project
;; ;; C-c p e   helm-projectile-recentf Lists recently opened files in current project
;; ;; C-c p a   helm-projectile-find-other-file Switch between files with same name but different extensions
;; ;; C-c p i   projectile-invalidate-cache Invalidate cache
;; ;; C-c p z   projectile-cache-current-file Add the file of current selected buffer to cache
;; ;; C-c p b   helm-projectile-switch-to-buffer List all open buffers in current project
;; ;; C-c p s g helm-projectile-grep Searches for symbol starting from project root
;; ;; C-c p s a helm-projectile-ack Same as above but using ack
;; ;; C-c p s s helm-projectile-ag Same as above but using ag
;; (load "pi-helm")

;; ;; ---------------
;; ;; * Enable smex *
;; (load "pi-smex")


;; ;; ---------------------
;; ;; * Enable projectile *
;; ;; Documentation here http://projectile.readthedocs.io/en/latest/
;; (load "pi-projectile")

;; ;; ---------------
;; ;; * docker-mode *
;; (load "pi-docker")

;; ;; See https://github.com/jml/direnv-el
;; ;; and http://direnv.net/
;; ;; ----------
;; ;; * direnv *
;; (load "pi-direnv")

;; ;; See https://github.com/iqbalansari/emacs-emojify
;; ;; -----------
;; ;; * emojify *
;; (load "pi-emojify")

;; ;; See https://github.com/gregsexton/origami.el
;; ;; super-tab to origami-toggle-node
;; ;; Shift-super-iso-lefttab origami-toggle-all-nodes
;; ;; -----------
;; ;; * origami *
;; (load "pi-origami")

;; ;; Elfeed Emacs Web Feed Reader
;; ;; https://github.com/skeeto/elfeed
;; ;; C-x w to start elfeed
;; ;; Add support to elfeed-goodies
;; ;; ----------
;; ;; * elfeed *
;; (load "pi-elfeed")

;; ;; Emacs url shortener
;; ;; https://github.com/yuyang0/url-shortener
;; ;; Define the aliase _gus for goo-url-shorten.
;; ;; You mus have an google api key and configuere the variable goo-api-key
;; ;; See https://developers.google.com/url-shortener/v1/getting_started#APIKey
;; ;; -----------------
;; ;; * url-shortener *
;; (load "pi-url-shortener")

;; ;; See https://github.com/DarthFennec/highlight-indent-guides
;; ;; ---------------------------
;; ;; * highlight-indent-guides *
;; (load "pi-indent-guides")

;; ;; Displays the key bindings following your currently entered incomplete command
;; ;; See https://github.com/justbur/emacs-which-key
;; ;; -------------
;; ;; * which-key *
;; (load "pi-which-key")

;; ;; Emacs search tool based on ripgrep
;; ;; See https://rgel.readthedocs.io
;; ;; ----------------
;; ;; * ripgrep tool *
;; (load "pi-rg")

;; ;; json-mode config
;; ;; --------------------
;; ;; * json-mode config *
;; (load "pi-json")


;; (when (require 'textile-mode nil t)
;;   (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode)))


(when pi-error-msgs
  (mapc (lambda (error-msg)
          (warn (concat "pimacs configuration error : " error-msg))
          ) pi-error-msgs)
  (error "\nPlease correct this warnings"))

;; (load (cuid "user-post-init"))
;; Local variables:
;; coding: utf-8
;; End:
