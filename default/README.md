# PIMacs -- Default

The default minimal configuration of Emacs by PIMacs.

## Setting Variables

* Fix missing `warning-suppress-types` function.
* Draw block cursor as wide as the glyph under it.
* Custom title Format.
* Replace the bell by visible blink.
* Don't use graphic dialog.
* Increase the number of macro recursions.
* Ignore the case in completion mode.
* Highlight the region when the mark is active.
* Save everything before compiling.
* Resize the compilation window.
* Scrolling by default the `*compilation*` buffer window as output appears.
* Enable local variables.
* Controls if scroll commands move point to keep its screen line unchanged.
* Maximum number of lines to keep in the message log buffer set to 1000.
* `apropos` command search all.
* Fill bulleted and indented lines (adaptive fill mode enabled).
* Do not add a new string to `kill-ring` when it is the same as the last one.
* Preferred split window horizontally
* Increase the number of bytes of consing between garbage collections because we have modern machines.
* Disable Global Auto-Revert Mode.
* `M-x proced` show all process, without filter by default.
* Show trailing whitespace and leading whitespace.
* Show no breaking char.
* Force Emacs to indent with spaces by default (customized for other language like Go).
* Activate Maximum coloration.
* Remove the line numbers in the margin (distraction-less).
* Natively compile packages as part of their installation (does not
  have any effect if Emacs was not built with native compilation support).
* Configure the fill column indicator.
* Prefer UTF-8 encoding system and language.
* Show by default the contextual function in the modeline (switching with `M-x which-function-mode`).
* Reconfigure smartly `grep-find-command` depending of the existence or not of [ripgrep(rg)](https://github.com/BurntSushi/ripgrep).  
  If the module `pimacs/aliases` is enabled, short alias of `grep-find` is `_rg` (let it a try).
* Enable matching whitespace literally (disable lax space matching).
  [By default searching two spaces also match single space](https://www.gnu.org/software/emacs/manual/html_node/emacs/Special-Isearch.html#Special-Isearch), who want this behavior ??

## Adding Hooks

* Ask for creating the directory when saving a buffer in an nonexistent directory.  
  `(add-hook 'before-save-hook …)`
* Abort the minibuffer when using the mouse. More information [here](https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html).  
  `(add-hook 'mouse-leave-buffer-hook 'pim-stop-using-minibuffer)`
* Enable `auto-fill-mode` in the hook pushed in the variable `pim-auto-fill-mode-hook-alist`.  
  Reconfigure `fill-nobreak-predicate` to ensure better break (none in `...` for example).
* Abort the minibuffer when using the mouse.
  See [this article](https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html) for the details.
  
## Mode Enabled

* Move cursor by camelCase : `global-subword-mode`.
* Selecting a region and inserting a new character will overwrite the whole region : `delete-selection-mode`.
* Show line/column number in the mode line : `line-number-mode` and `column-number-mode`.
* Read automatically  `.gz` and `.zip` files : `auto-compression-mode`.
* Read images by default : `auto-image-file-mode`.
* Enable whitespace visualization globally : `global-whitespace-mode`.
* Enables coloring in all modes : `global-font-lock-mode`.
* Display fill column indicator in all modes : `global-display-fill-column-indicator-mode`.

## Customizing Doom It-self

* Make only the minibuffer as unreal. Use `C-x B` to see all the buffers.  
  Doom consider some buffers as unreal buffer (like *Message*) but I need to see these buffers !!

## Useful Interactive Functions

* `pim/scissor` : Insert a line of `pim/scissor-pattern` in the buffer.
  
  `✂····✂····✂····✂····✂····✂····✂····✂····✂····✂····✂····`

## Initialized and Configured Packages

* `htmlize-view` : convert buffer/region/file to html, preserving colors and decoration and view in web browser.
* `tramp` : tramp access functions for (s)sh-like connections.
* `boxquote` : provides a set of functions for using a text quoting
  style that partially boxes in the left hand side of an area of text,
  such a marking style might be used to show externally included text
  or example code. Usage : `M-x boxquote-…`

