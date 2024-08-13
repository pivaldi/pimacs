# PIMacs -- Session

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
