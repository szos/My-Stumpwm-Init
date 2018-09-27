(in-package :stumpwm)

(defmacro define-hydra-macro (map key &rest bindings)
  "this macro takes a map, a key, and some more bindings, and generates a hydra
which lets you create something like a prefix key. for example if you want to 
bind a command to 'prefix C-x n' youd write 

;;(define-hydra *root-map* (kbd \"C-x\")
;;  ((kbd \"n\") command)) 

this lets you create hydras for related behavior. "
  (let* ((m (gensym))
	 (bind-to-m (mapcar #'(lambda (bind)
				(push m bind)
				(push 'define-key bind))
			    bindings)))
    `(define-key ,map ,key
       (let ((,m (make-sparse-keymap)))
	 ,@bind-to-m
	 ,m))))

(defmacro defhydra (&rest bindings)
    (let* ((m (gensym))
	   (bind-to-m (mapcar #'(lambda (bind)
				  (push m bind)
				  (push 'define-key bind))
			      bindings)))
      `(let ((,m (make-sparse-keymap)))
	 ,@bind-to-m
	 ,m)))

(defmacro define-lambda-hydra (&rest bindings)
  (let ((bind-to-m (mapcar #'(lambda (bind)
			       (push 'm bind)
			       (push 'define-key bind))
			   bindings)))
    `(let ((m (make-sparse-keymap)))
       ,@bind-to-m
       m)))

(defcommand define-hydra (bindings)
    ((:rest "bindings: "))
  (let ((k (make-sparse-keymap)))
    (when  (stringp bindings)
      (setf bindings (read-from-string bindings)))
    (mapcar #'(lambda (binding)
		(funcall #'define-key k (eval (car binding)) (second binding)))
	    bindings)
    k))

(defmacro defer-hydra-macro (bindings)
  (let ((k (gensym)))
    `(let ((,k (make-sparse-keymap))
	   (bb ',bindings))
       (when (stringp bb)
	  (setf bb (read-from-string bb)))
       (mapcar #'(lambda (binding)
		   (funcall #'define-key ,k (eval (car binding)) (second binding)))
	       bb)
       ,k)))

(defcommand redef-top () ()
  (define-key *top-map* (kbd "C-q")
    *top-redef*))

(defparameter *top-redef*
  (defhydra
      ((kbd "l") "slimeball")
      ((kbd "r") "replball")
    ((kbd "n") "notes")
    ((kbd "f") "access-floats")
    ((kbd "F") "access-floats-global")
    ((kbd "s") "snap-floating-windows")
    ((kbd "m") "sys-maniper")
    ((kbd "q") "meta q")
    ((kbd "t") (defhydra
		   ((kbd "t") "toggle-always-on-top")
		   ((kbd "g") "toggle-always-show")))))

(define-key *top-map* (kbd "C-M-s-q")
  *top-redef*)

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

(define-key *root-map* (kbd "s")
  (define-hydra
      '(((kbd "v") "hsplit")
	((kbd "h") "vsplit"))))

;; (define-key *root-map* (kbd "C-s") "hsplit-equally")
;; (define-key *root-map* (kbd "M-s") "vsplit-equally")
;; (define-key *root-map* (kbd "s") "hsplit-equally 2")
;; (define-key *root-map* (kbd "S") "vsplit-equally 2")
(undefine-key *root-map* (kbd "C-S"))
(define-key *root-map* (kbd "M-q") "system-manipulation")
(define-key *root-map* (kbd "M-b") "move-focus left")
(define-key *root-map* (kbd "M-f") "move-focus right")
(define-key *root-map* (kbd "M-n") "move-focus down")
(define-key *root-map* (kbd "M-p") "move-focus up")
(define-key *root-map* (kbd "m") "modeline-stumptray-toggle")

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
;; (define-key *root-map* (kbd "w") "pull-from-windowlist-formatted") ;; possibly missing/unused. 
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
(define-key *root-map* (kbd "r") "remove")
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
(define-key *root-map* (kbd "space") "command-mode")
(define-key *root-map* (kbd "B") "brightness-map")
(define-key *root-map* (kbd "y") "kill-yt")

;; define our modal control keybind - this is special.
;; (define-key *top-map* (kbd "C-o") "modal-controller")
;; volume

;; define system keys. 
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume -5")
(define-key *top-map* (kbd "XF86AudioMute") "vol-reset")

(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "volume-overdrive 200")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "volume-overdrive 150")
(define-key *top-map* (kbd "M-XF86AudioMute") "volume-overdrive 100")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-change -1")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-change 1")
(define-key *top-map* (kbd "SunPrint_Screen") "screenshot")

;; (define-key *top-map* (kbd "s-:") "window-send-string Ø")
;; (define-key *top-map* (kbd "s-;") "window-send-string \"ø\"")

;; øØ æÆ åÅ provided by xmodmap
(define-key *root-map* (kbd "C-d") "describe-key");;


;;;; load translation keys
(load-module "translation-keys")

(translation-keys:define-key-translations "vlc"
    (("C-p" "meta Up"
	    ("Increase Volume"))
     ("C-n" "meta Down"
	    ("Decrease Volume"))
     ("C-f" "meta S-Right"
	    ("FF 3 seconds"))
     ("C-b" "meta S-Left"
	    ("RW 3 seconds"))
     ("M-f" "meta M-Right"
	    ("FF 10 seconds"))
     ("M-b" "meta M-Left"
	    ("RW 10 seconds"))
     ("C-M-f" "meta C-Right"
	      ("FF 1 Minute"))
     ("C-M-b" "meta C-Left"
	      ("RW 1 Minute"))
     ;; ("C-x C-f" "meta C-o"
     ;; 		("open media"))
     ("C-RET" "meta n"
	      ("Next File in Playlist"))))

(translation-keys:define-key-trans "Tor Browser"
    `(("C-g" "meta ESC"
	     ("quit/escape"))
      ("M-<" "meta Home"
     	     ("go to top of page"))
      ("M->" "meta End"
     	     ("go to bottom of page"))
      ("C-v" "meta SunPageDown"
     	     ("Page down"))
      ("M-v" "meta SunPageUp"
     	     ("page up"))
      ("M-w" "meta C-c"
     	     ("copy selected text"))
      ("C-w" "meta C-x"
     	     ("cut selected test"))
      ("C-y" "meta C-v"
     	     ("paste selected text."))
      ("C-s" "meta C-g"
     	     ("search forward."))
      ("C-r" "meta C-G"
     	     ("search backward"))
      ("C-n" "meta Down"
     	     ("send a down arrow key. figure out how to differentiate..."))
      ("C-p" "meta Up"
     	     ("send an up arrow"))
      ("C-M-b" "meta C-["
     	       ("navigate backwards one page in history"))
      ("C-M-f" "meta C-]"
      	       ("navigate forwards one page in history"))
      ("M-f" "meta C-TAB"
	     ("move forward one tab"))
      ("M-b" "meta C-S-TAB"
	     ("move backward one tab"))
      ("M-B" "meta C-S-SunPageUp"
	     ("pull current tab left"))
      ("M-F" "meta C-S-SunPageDown"
	     ("Pull current tab right"))
      ;; ("M-r" "ratcontrol-typing"
      ;; 	     ("starts ratcontrol without <RET> bound to exit interactive keymap."))
      ("M-r" "ratsnap"
	     ("hyper rat. prompt for direction and ammount"))
      ("H-j" "ratsnap left 75"
	     ("hyper rat. move pointer left by 75 pixles."))
      ("H-k" "ratsnap down 75"
	     ("hyper rat. move pointer down by 75 pixles."))
      ("H-l" "ratsnap right 75"
	     ("hyper rat. move pointer right by 75 pixles"))
      ("H-i" "ratsnap up 75"
	     ("hyper rat. move pointer up by 75 pixles."))
      ("H-J" "ratsnap left 20"
	     ("hyper rat. move pointer left by 20 pixles."))
      ("H-K" "ratsnap down 20"
	     ("hyper rat. move pointer down by 20 pixles."))
      ("H-L" "ratsnap right 20"
	     ("hyper rat. move pointer right by 20 pixles"))
      ("H-I" "ratsnap up 20"
	     ("hyper rat. move pointer up by 20 pixles."))
      ("H-SPC" "ratclick 1"
	       ("ratclick left click"))
      ("H-s-SPC" "ratclick 2"
		 ("ratclick right click"))
      ("s-SPC" "ratclick 3"
	       ("ratclick middle click"))
      ("C-x" ,(define-hydra
		  '(((kbd "C-c") "meta C-q") ;; quit
		    ((kbd "k") "meta C-w") ;; close tab
		    ;; ((kbd "k") "kill-prompt \"close window?: \" \"meta C-w\"")
		    ((kbd "K") "meta C-S-w") ;; close window
		    ((kbd "1") "meta C-1") 
		    ((kbd "2") "meta C-2")
		    ((kbd "u") "meta C-T") ;; undo close tab
		    ((kbd "C-f") "meta '") ;; search text in links
		    ((kbd "n") "meta C-n") ;; new window
		    ((kbd "p") "meta C-S-p") ;; new private window
		    ((kbd "x") "meta F6") ;; cycle browser frame
		    ((kbd "C-x") "meta S-F6")
		    ((kbd "+") "meta C-+") ;; zoom in 
		    ((kbd "-") "meta C--") ;; zoom out
		    ((kbd "=") "meta C-0") ;; reset zoom
		    ((kbd "0") "meta C-0")))
	     ("this is a hydra, defining C-c as quit, etc..."))))
