# Stumpwm Init Files #

This is a home for my .stumpwm.d/ directory

## init.lisp ##

This houses general set up, quickloads librarys, loads modules, sets up swank

## keybindings.lisp ##

defines keybindings, as well as a hydra macro, also contains command for selecting the search bar of various websites - ff-focus-search-bar.

## utilities.lisp ##

Contains utilites, as well as closures for controlling volume, screen temperature, brightness, etc.

## with-open-window.lisp ##

Defines functionality for doing something with a window once it opens. For example changing the class of a window when it opens. All functions passed to this need to take at least one argument - the current window.

## fuzzy-finder.lisp ##

Allows us to search for windows using partial portions of class, title, etc, instead of requiring the exact class, title, etc.

## commands.lisp ##

Contains commands, as well as a shitty cond-let, which isnt really a cond-let, more like a let-cond (look at the def).

## applications.lisp ##

Contains bindings for applications, sorted by category.

## sibling.lisp ##

Contains the commands sib and remove-sibling (and their plumbing). These commands operate on the closest sibling, and is designed to get around shortcomings in the sibling command.

## scheme-macros.lisp ##

contains an implementation of scheme R5RS macros - specifically define-syntax, let syntax, and letrec-syntax. This was shamelessly taken from https://www.ccs.neu.edu/home/dorai/mbe/mbe-lsp.html as it is licensed under the LGPL, as seen here https://ds26gte.github.io/index.html. Please thank Dorai Sitaram for the implementation - he is the author. 