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
 ;; keys ;; Define the PIMacs keys binding. See the `refcard.md`
 ;; session ;; Auto reload the last session at startup through
          ;;;; `doom/quickload-session` (no desktop bad practice in Doom)

 ;; lang-c
 ;; lang-lisp
 ;; lang-make
 )
```

**Remember to run `doom sync` after modifying `init.el` !**

## Workspace/Session Persistence and Auto Restore Last Session

The PIMacs module `:pimacs session` permits to automatically restore
your session and workspaces. Here how to proceed :

* Create a workspace named `default` by creating a standard workspace #x (`+workspace/new C-c w c`)
  and rename it `default` (*DON'T use `+workspace/new-named C-c w C`*).
* Open files in this workspace.
* Create others workspaces as needed with `+workspace/new`
* Close Emacs

When opening again Emacs, you retrieve the opened buffers in the `default`
workspaces and the others workspaces are available by `C-c w w` or
`s-tab` if the module `:pimacs keys` is enabled.
