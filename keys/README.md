# PIMacs/Keys Doom Module

This module configure useful key bindings in `global-map`.  
See [the key bindings refcard added by this module](keys-key-bindings-refcard.org)
with **`+azerty` keyboard**.

## Notes

- This module remove the Doom advice on the `newline-and-indent` function for
  continuing comment on newline ; use instead `M-<RET>` for this feature.
- This module remap the `dabbrev-expand` key binding to `hippie-expand`.
  See [Text Expansion with Hippie Expand](https://www.masteringemacs.org/article/text-expansion-hippie-expand).
- You can generate the refcard of all PIMacs key bindings for your own
  configuration with the command `pim-keys-bindings-to-refcard`.
- With a /qwerty/ keyboard, the key bindings will be different !

**Pull request is welcome to provide other keyboard support, evil support or improve the existing.**
