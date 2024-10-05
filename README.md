# PIMacs -- WORK IN PROGRESS !

## Extended Configuration of Emacs on Top of Doom Emacs

* **THIS IS A WORK IN PROGRESS, DON'T USE IT FOR NOW !**
* This is a huge refactoring of [`pi-emacs-configuration`](https://github.com/pivaldi/pi-emacs-configuration) using [Doom Emacs](https://github.com/doomemacs/doomemacs)

## Installation

* Clone this repository into your `USERDOOMDIR/modules` (`~/.doom.d/modules` on Linux) :  
  `cd ~/.doom.d/modules && git clone https://github.com/pivaldi/pimacs.git`
* You can enable PIMacs in two ways :  
  1. **Full featured PIMacs as it satisfy MY daily coding needs**  
     In your Doom file `init.el`, at the end of the file, after the `(doom! …)`  
     part, add the code `(load! "modules/pimacs/init")`.  
     This will load *all the PIMacs modules* at once.
  1. **Partially featured PIMacs as it satisfy YOUR needs**  
     In your Doom file `init.el`, at the end of the file, after the `(doom! …)`  
     part, add the following code uncommenting the module you want to enabled :

```elisp
(doom! :pimacs
 default ;; PIMacs default Emacs configuration on top of Doom.
 ;; aliases ;; Define aliases starting with `_`. The most useful are `_rb`, `_sir`, `_gf`.
 ;; (theme +no-font) ;; Define some faces and load Zenburn Theme.
                   ;;;; See the `README.md` in the directory `pimacs/theme`
 ;; (keys +azerty) ;; Define the PIMacs keys binding.
 ;; session ;; Auto reload the last session at startup through
          ;;;; `doom/quickload-session` (no desktop bad practice in Doom)

 ;; lang-c
 ;; lang-lisp
 ;; lang-make
 
 ;; …
 
 )
```

**Remember to run `doom sync` after modifying `init.el` !**

## Modules

### pimacs/default
[PIMacs default configuration](default/README.md).

### pimacs/keys
[PIMacs `global-map` configuration](keys/README.md).  
See also the auto-generated
[global-map key bindings refcard](keys/all-key-bindings-refcard.org) and the
[global-map PIMacs key bindings refcard](keys/pimacs-key-bindings-refcard.org).

### pimacs/pairing
[Enhanced configuration of smartparens with extended features](pairing/README.org).

### pimacs/aliases
[Define useful aliases starting by `_`](aliases/config.el).

### pimacs/functions
[Define useful functions used under the hood by PIMacs](functions/README.md).

### pimacs/session
[Workspace/Session Persistence and Auto Restore Last Session](session/README.md).

## pimacs/bm
[Visible local bookmarks in buffer](bm/README.org) is a simple and useful
complement to the native Emacs bookmark system.

## pimacs/origami
[Provide/configure the Origami text folding minor mode](origami/README.org)

### pimacs/dired
[Dired mode specific configuration](dired/README.org).  
See also the auto-generated
[dired-mode-map key bindings refcard](dired/all-key-bindings-refcard.org) and the
[dired-mode-map PIMacs key bindings refcard](dired/pimacs-key-bindings-refcard.org).

### pimacs/doc
[Generated PIMacs Documentation](doc/README.org). Contains all the Doom and
PIMacs fundamental key bindings refcards classed by key bindings prefixes.

<!-- ## External Tools -->

<!-- This is a list of external tools you should install which are automatically used -->
<!-- when detected ! -->

<!-- - [Semgrep](https://semgrep.dev/docs/) : Find bugs and reachable dependency -->
<!--   vulnerabilities in code.   -->
<!--   You may use the command line `pipx install semgrep` to install it. -->
