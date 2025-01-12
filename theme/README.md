# PIMacs/theme Doom Module

Slight customization of the Emacs theme depending the module options used.

* Use the first font found in this order :
  1. `Source Code Pro` : https://github.com/adobe-fonts/source-code-pro
  2. `Terminus` : https://terminus-font.sourceforge.net/
  3. `Hack` : https://github.com/source-foundry/Hack?tab=readme-ov-file
  4. `DejaVu Sans Mono` : https://dejavu-fonts.github.io/
  5. nothing
* (Re)Define some faces, `trailing-whitespace` for example (use option
  `+no-whitespace-style` to disable this feature).
* Load a slightly customized [Zenburn Theme](https://github.com/bbatsov/zenburn-emacs) (use option
  `+no-zenburn-theme` to disable this feature).
* Disable `hl-line` in non special modes.
* Remove line numbering because we have this information into the mode-line.

Module options are :
* `+no-font` : do not modify the default font.
* `+no-zenburn-theme` : do not use [Zenburn Theme](https://github.com/bbatsov/zenburn-emacs).
* `+no-whitespace-style` : do not modify the default whitespace style.
