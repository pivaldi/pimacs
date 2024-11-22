# PIMacs

## Extended Configuration of Emacs on Top of Doom Emacs

Here an extended configuration of Emacs 29+ on Top of Doom Emacs programming
oriented with special attention to French keyboard.

* **This is a work in progress** but I use daily this configuration for
  professional programming and administration system works.  
  See the roadmap at the end of this file and the [to-do list](TODO.org).
* This is a huge refactoring of [`pi-emacs-configuration`](https://github.com/pivaldi/pi-emacs-configuration) using [Doom Emacs](https://github.com/doomemacs/doomemacs)

## Dependencies

You imperatively need these softwars installed :

* The last stable [Doom Emacs](https://github.com/doomemacs/doomemacs)
* [Emacs 29+](https://www.gnu.org/software/emacs/manual/html_node/efaq/New-in-Emacs-29.html)
  compiled with [Tree-sitter](https://tree-sitter.github.io/tree-sitter/)
  support.  
  [Here](https://gist.github.com/pivaldi/116c22742040834316c90e938411c082) a
  bash script example to compile and install an optimized Emacs 29+ version with
  Tree-sitter support.

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
 
 ;; etc…
 
 )
```

**Remember to run `doom sync` after modifying `init.el` !**

## Modules

### pimacs/aliases
[Define useful aliases starting by `_`](aliases/config.el).

### pimacs/avy
[Create key bindings for avy with prefix (Doom does not provide them)](avy/README.org).

## pimacs/bm
[Visible local bookmarks in buffer](bm/README.org) is a simple and useful
complement to the native Emacs bookmark system.

### pimacs/calendar-fr

Configure the calendar for French support.

### pimacs/corfu

[Extra configuration of the package COmpletion in Region FUnction](corfu/README.org)

### pimacs/default
[PIMacs default configuration](default/README.md).

### pimacs/dired
[Dired mode specific configuration](dired/README.org).  
See also the auto-generated
[dired-mode-map key bindings refcard](dired/all-key-bindings-refcard.org) and the
[dired-mode-map PIMacs key bindings refcard](dired/pimacs-key-bindings-refcard.org).

### pimacs/doc
[Generated PIMacs Documentation](doc/README.org). Contains all the Doom and
PIMacs fundamental key bindings refcards classed by key bindings prefixes.

### pimacs/functions
[Define useful functions used under the hood by PIMacs](functions/README.md).

### pimacs/keys
[PIMacs `global-map` configuration](keys/README.md).  
See also the auto-generated
[global-map key bindings refcard](keys/all-key-bindings-refcard.org) and the
[global-map PIMacs key bindings refcard](keys/pimacs-key-bindings-refcard.org).

### pimacs/lang-c

Configuration of `C` mode and derived.

``` elisp
  (setq-default c-basic-offset 2)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'brace-list-open '0)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'statement-case-open '0)
  (c-set-offset 'arglist-cont-nonempty '4)
  (c-set-offset 'arglist-intro 'c-basic-offset)
```

### pimacs/lang-go

[Configuration for the Go programming language](lang-go/README.org).  
Support native `go-ts-mode` (Emacs 30+) and standart `go-mode`.

### pimacs/lang-php

[Configuration for the PHP programming language](lang-php/README.org).  
Support native `php-ts-mode` (Emacs 29+) and standart `php-mode`.

### pimacs/lsp

Complet/extend the default Doom configuration of the [Langage Server Protocol](https://microsoft.github.io/language-server-protocol/).

## pimacs/origami
[Provide/configure the Origami text folding minor mode](origami/README.org)

### pimacs/pairing
[Enhanced configuration of smartparens with extended features](pairing/README.org).

### pimacs/session
[Workspace/Session Persistence and Auto Restore Last Session](session/README.md).

### pimacs/theme

[Slight customization of the Emacs theme depending the module options used](theme/README.md)

### pimacs/tramp

[Slight configuration of TRAMP](tramp/README.md).  
`TRAMP` is a built-in Emacs feature that enables you to access and edit files on
remote systems, including those that require authentication using `SSH`, `SFTP`, or
other protocols.

### pimacs/treesit

Automatically install and use tree-sitter major modes in Emacs 29+.


## Roadmap

* Module for sql-ts-mode/sql-mode
* Better typescript-ts-mode
* Snippet
* Templating
* Support of Go html template
* Hide password on some files (gpg, authinfo, etc)
* Check macro support of Doom
* Improve dired experience
* Make a module for direnv support
* Use easy-kill https://github.com/leoliu/easy-kill
* elfeed
* lang-lua
* lang-rust
* lang-asy
* Configure org-mode and markdown support for my needs

<!-- ## External Tools -->

<!-- This is a list of external tools you should install which are automatically used -->
<!-- when detected ! -->

<!-- - [Semgrep](https://semgrep.dev/docs/) : Find bugs and reachable dependency -->
<!--   vulnerabilities in code.   -->
<!--   You may use the command line `pipx install semgrep` to install it. -->
