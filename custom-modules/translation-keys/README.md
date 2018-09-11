This is a method of redefining keys in the top map dynamically so as to
rebind keybinds in applications that dont allow rebinding. If you have
anything defined in the top map that gets bound in a translation keys
define-key-translations will be overwritten. to fix this, add those
keybindings to a command ```redef-top``` which takes no arguments.
if that command (or function) exists, it will be called when unbinding
the key translations to add your default bindings. 