This module provides a macro ```DEFINE-TRANSLATION-KEYS``` which takes a
window class (as a string), and a set of keymaps in the format of
```'(((kbd "C-w") "meta C-x")
     ((kbd "C-y") "meta C-v"))```
These keybindings will get dynamically bound, depending on the class.

Because this binds keys in ```*top-map*```, any keybindings that are
shared between key translations and the top map will end up unbound.
To avoid this, define a command or function in the stumpwm package
called ```redef-top``` containing all top map key bindings. If this
function exists it will be called after unbinding key translations.
It should take no arguments. 

You can find an example of this in ```keybindings.lisp```, using quasiquotes
to generate one shot hydras within ```redef-top```. 