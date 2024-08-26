# PIMacs/functions Doom Module

Useful interactive functions used by PIMacs.  
See the docstrings of each function for furthers informations.

* `pim-make-script-executable` : if file start with a shebang, make buffer file name executable.
* `pim/kill-and-join-forward` : if at end of line, join with following, otherwise kill line.
* `pim/buffer-file-name` : show the buffer file name (if any) and make it the latest kill in the kill ring if KILLIT is t.
* `pim/backward-delete-word` : delete characters backward until encountering the beginning of a word.
  With argument ARG, do this that many times.
* `pim/delete-sexp` : delete the sexp (balanced expression) following point.
* `pim/backward-delete-sexp` : delete the sexp (balanced expression) preceding point.
* `pim/kill-window-and-buffer` : delete current window and buffer.
* `pim/indent-whole-html-buffer` : indent the whole buffer except `<pre>` part in html mode.
* `pim/indent-whole-buffer` : indent the whole buffer.
* `pim/find-file-root` : find file as root.
* `pim/home()` : move cursor at beginning of line or first non blank character depending where the cursor is.
* `pim/?comment` : comment/Uncomment the entire line and indent if arg INDENTP is t.
* `pim/insert-comment-section` : insert a section comments.
* `pim/insert-comment-sub-section` : insert a section sub comments.
* `pim/fill` : use fill line or region as `auto-fill-mode` does.
* `pim/insert-semicol-at-end-of-line` : smartly insert a semicolumn at the end of the line.
* `pim/insert-comma-at-end-of-line` : smartly insert a comma at the end of the line.
* `pim/next-user-buffer` : switch to the next real buffer.
* `pim/previous-user-buffer` : switch to the previous real buffer.
* `pim/keys-bindings-to-refcard` : Export the PIMacs key bindings from keymaps in a file (org,
  md and txt format supported).
* `pim/modules-key-bindings-to-refcard` : Export the PIMacs key bindings from
  module names in a file (org, md and txt format supported).
