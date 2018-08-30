(in-package :stumpwm)

(defmacro define-hydra (map key &rest bindings)
  "this macro takes a map, a key, and some more bindings, and generates a hydra
which lets you create something like a prefix key. for example if you want to 
bind a command to 'prefix C-x n' youd write 

(define-hydra *root-map* (kbd \"C-x\")
  ((kbd \"n\") command)) 

this lets you create hydras for related behavior. "
  (let ((bind-to-m (mapcar #'(lambda (bind)
			       (push 'm bind)
			       (push 'define-key bind))
			   bindings)))
    `(define-key ,map ,key
       (let ((m (make-sparse-keymap)))
	 ,@bind-to-m
	 m))))

(define-hydra *top-map* (kbd "C-q")
  ((kbd "l") "slimeball")
  ((kbd "n") "notes")
  ((kbd "f") "access-floats")
  ((kbd "F") "access-floats-global")
  ((kbd "m") "sys-maniper")
  ((kbd "q") "meta q"))

(defcommand sys-maniper () ()
  (system-manipulation)
  (sys-manip-help))

(define-interactive-keymap brightness-map ()
  ((kbd "=") "brightness-change 1")
  ((kbd "+") "brightness-change 1")
  ((kbd "-") "brightness-change -1")
  ((kbd "_") "brightness-change -1")
  ((kbd "p") "brightness-change 1")
  ((kbd "n") "brightness-change -1"))

;; Read some doc
(define-key *root-map* (kbd "d") "exec gv")
;; window
(define-key *root-map* (kbd "C-'") "gnext-map")

;; (define-key *root-map* (kbd "f") "notes")
;; (define-key *root-map* (kbd "C-f") "access-floats")
;; (define-key *root-map* (kbd "M-f") "access-floats-global")

(define-hydra *root-map* (kbd "s")
  ((kbd "v") "vsplit-equally 2")
  ((kbd "h") "hsplit-equally 2")
  ((kbd "V") "vsplit-equally")
  ((kbd "H") "hsplit-equally"))

;; (define-key *root-map* (kbd "C-s") "hsplit-equally")
;; (define-key *root-map* (kbd "M-s") "vsplit-equally")
;; (define-key *root-map* (kbd "s") "hsplit-equally 2")
;; (define-key *root-map* (kbd "S") "vsplit-equally 2")
(define-key *root-map* (kbd "M-q") "system-manipulation")
(define-key *root-map* (kbd "M-b") "move-focus left")
(define-key *root-map* (kbd "M-f") "move-focus right")
(define-key *root-map* (kbd "M-n") "move-focus down")
(define-key *root-map* (kbd "M-p") "move-focus up")

;; Browse somewhere
;; (define-key *root-map* (kbd "b") "colon1 exec firefox http://www.")
;; Ssh somewhere
;; (define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh ")
;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock")
;;call grouplist to switch groups (workspaces)
(define-key *root-map* (kbd "g") "grouplist")
;;jump straight to eval prompt
(define-key *root-map* (kbd "M-e") "eval")
;;change behavior of prefix-w (normally calls windows) to call windowlist isntead
(define-key *root-map* (kbd "w") "pull-from-windowlist-formatted")
(define-key *root-map* (kbd "C-w") "vgroups")
;;define the menu key from app-menu module
(define-key *root-map* (kbd "j") "show-menu")
;;define key for showing clipboard history
;; (define-key *root-map* (kbd "C-y") "show-clipboard-history")
;;implement our window and group hoppers
;; (define-key *root-map* (kbd "M-w") "window-hop")
;; (define-key *root-map* (kbd "M-j") "group-hop")
;; Implement remove split, maybe remove split horizontal and 
;; vertical
(define-key *root-map* (kbd "C-r") "balanced-remove-split") ; removes split and switches to previous 
;; (define-)
;; switching layouts
;; (define-key *root-map* (kbd "C-l") "change-kb-layout")
;; modal mouse controller:
(define-key *root-map* (kbd "M-r") "ratcontrol")
;; expose all windows in group
(define-key *root-map* (kbd "C-e") "expose")
;;brightness:
;; (define-key *root-map* (kbd "-") "brightness-")
;; (define-key *root-map* (kbd "=") "brightness+")
;; volume:
(define-key *root-map* (kbd "v") "volume") 

;;;;; TOP MAP BINDINGS
;; cycle groups
(define-key *root-map* (kbd "C-M-f") "gnext")
(define-key *root-map* (kbd "C-M-b") "gprev")
(define-key *root-map* (kbd "space") "gnext")
(define-key *root-map* (kbd "B") "brightness-map")

;; define our modal control keybind - this is special.
;; (define-key *top-map* (kbd "C-o") "modal-controller")
;; volume

;; define system keys. 
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume 10")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume -10")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-change -1")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-change 1")

(define-key *root-map* (kbd "C-d") "describe-key");;
