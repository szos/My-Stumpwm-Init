# My-Stumpwm-Init #

This is a home for my .stumpwm.d/ directory.

## ratcontrol and translation-keys ##
(housed in custom-modules) Modules I've written for controlling the pointer and simulating keys a la EXWM. translation-keys needs some work, the current kludge is to bind everything to *top-map* dynamically.

## appmenu-*.lisp ##
menu files for the appmenu module.

## commands-reclass.lisp ##
This was the first approach at with-open-windows.lisp, and is only here for my own uses (feel free to use it, but it wont be helpful).

## commands-utilities.lisp ##
This is a set of closures for controlling volume, brightness, key layout, mode line, screen temperature, and an interactive keymap for controlling it all.

## commands.lisp ##
Contains a bunch of commands relative to my installed software. it also has a lot of abandoned functions that I need to clean up.

## expose.lisp ##
contains the expose functionality.

## init.lisp ##
is the actual init file.

## keybindings.lisp ##
contains all keybindings as well as a small macro for defining hydras - similar to define-interactive-keymap, except its not interactive, its just binding multiple additional keys to a keybinding, allowing C-x i to be an acceptable binding.

## windows-groups-frames.lisp ##
contains a semi-fuzzy window searching mechanism as well as some commands for manipulating windows - mostly just testing functionality.

## with-open-window.lisp ## allows one to run a shell command or stumpwm command that either raises or opens a window, and then do things to that window. This is useful for things like automatically floating a window. 