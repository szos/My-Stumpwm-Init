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


(define-key *root-map* (kbd "C-d") "describe-key")

(define-key *root-map* (kbd "'") "pullstr")

;; define our modal control keybind - this is special.
;; (define-key *top-map* (kbd "C-o") "modal-controller")
;; volume

(load-module :simkey)
(add-hook *focus-window-hook* 'simkey:trans-keys-hangar)

(simkey:define-top-map-key (kbd "C-;") *root-map*)

;; define system keys.
(simkey:define-top-map-key (kbd "XF86AudioRaiseVolume") "volume 5")
;; (define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume 5")
(simkey:define-top-map-key (kbd "XF86AudioLowerVolume") "volume -5")
;; (define-key *top-map* (kbd "XF86AudioLowerVolume") "volume -5")
(simkey:define-top-map-key (kbd "XF86AudioMute") "vol-reset")
;; (define-key *top-map* (kbd "XF86AudioMute") "vol-reset")

(simkey:define-top-map-key (kbd "M-XF86AudioMute") "volume-overdrive 100")
(simkey:define-top-map-key (kbd "M-XF86AudioRaiseVolume") "volume-overdrive 200")
(simkey:define-top-map-key (kbd "M-XF86AudioLowerVolume") "volume-overdrive 150")

(simkey:define-top-map-key (kbd "XF86MonBrightnessDown") "brightness-change -1")
(simkey:define-top-map-key (kbd "C-XF86MonBrightnessDown") "brightness-change 0")

(simkey:define-top-map-key (kbd "XF86MonBrightnessUp") "brightness-change 1")
(simkey:define-top-map-key (kbd "C-XF86MonBrightnessUp") "brightness-change 2")

;; (simkey:define-top-map-key (kbd "M-XF86MonBrightnessUp") "brightness-change ")

(simkey:define-top-map-key (kbd "SunPrint_Screen") "screenshot")

;; (define-key *top-map* (kbd "s-:") "window-send-string Ø")
;; (define-key *top-map* (kbd "s-;") "window-send-string \"ø\"")

;; øØ æÆ åÅ provided by xmodmap
;;

;;(load-module "translatin;; g-tests")
;; (translating-tests:def-trans-keys "Firefox"
;;     ((kbd "C-n") "meta Down")
;;   ((kbd "C-p") "meta Up"))

(simkey:define-key-translation "Firefox"
  ((kbd "C-g") "meta ESC")
  
  ((kbd "C-v") "meta SunPageDown")
  ((kbd "M-v") "meta SunPageup")
  ((kbd "M-<") "meta Home")
  ((kbd "M->") "meta End")
  
  ((kbd "C-p") "meta Up")
  ((kbd "C-n") "meta Down")
  
  ((kbd "C-s") "meta C-g")
  ((kbd "C-r") "meta C-G")
  ((kbd "M-s") "meta '")

  ((kbd "C-w") "meta C-x")
  ((kbd "M-w") "meta C-c")
  ((kbd "C-y") "meta C-v")

  ((kbd "M-f") "meta C-TAB")
  ((kbd "M-b") "meta C-S-TAB")
  ((kbd "M-B") "meta C-S-SunPageUp")
  ((kbd "M-F") "meta C-S-SunPageDown")

  ((kbd "C-x") (define-hydra
		   '(((kbd "k") "meta C-w") ;; close tab
		     ((kbd "K") "meta C-S-w") ;; close window
		     ((kbd "u") "meta C-T") ;; undo close tab
		     ((kbd "C-f") "meta '") ;; search links
		     ((kbd "n") "meta C-n") ;; new window
		     ((kbd "p") "meta C-P") ;; new private window
		     ((kbd "X") "meta F6")
		     ((kbd "x") "duckduckgo-focus-text")
		     ((kbd "+") "meta C-+")
		     ((kbd "-") "meta C--")
		     ((kbd "0") "meta C-0")

		     ((kbd "b") "meta C-[")
		     ((kbd "f") "meta C-]"))))
  ;; hyper rat
  ((kbd "M-r") "ratsnap")
  ((kbd "H-j") "ratsnap left 75")
  ((kbd "H-k") "ratsnap down 75")
  ((kbd "H-l") "ratsnap right 75")
  ((kbd "H-i") "ratsnap up 75")
  ((kbd "H-s-j") "ratsnap left 20")
  ((kbd "H-s-k") "ratsnap down 20")
  ((kbd "H-s-l") "ratsnap right 20")
  ((kbd "H-s-i") "ratsnap up 20")
  ((kbd "H-SPC") "ratclick 1")
  ((kbd "H-s-SPC") "ratclick 2")
  ((kbd "s-SPC") "ratclick 3"))

(simkey:define-key-translation "Tor"
  ((kbd "C-g") "meta ESC")
  
  ((kbd "C-v") "meta SunPageDown")
  ((kbd "M-v") "meta SunPageup")
  ((kbd "M-<") "meta Home")
  ((kbd "M->") "meta End")
  
  ((kbd "C-p") "meta Up")
  ((kbd "C-n") "meta Down")
  
  ((kbd "C-s") "meta C-g")
  ((kbd "C-r") "meta C-G")
  ((kbd "M-s") "meta '")

  ((kbd "C-w") "meta C-x")
  ((kbd "M-w") "meta C-c")
  ((kbd "C-y") "meta C-v")

  ((kbd "M-f") "meta C-TAB")
  ((kbd "M-b") "meta C-S-TAB")
  ((kbd "M-B") "meta C-S-SunPageUp")
  ((kbd "M-F") "meta C-S-SunPageDown")

  ((kbd "C-x") (define-hydra
		   '(((kbd "k") "meta C-w") ;; close tab
		     ((kbd "K") "meta C-S-w") ;; close window
		     ((kbd "u") "meta C-T") ;; undo close tab
		     ((kbd "C-f") "meta '") ;; search links
		     ((kbd "n") "meta C-n") ;; new window
		     ((kbd "p") "meta C-P") ;; new private window
		     ((kbd "X") "meta F6")
		     ((kbd "x") "duckduckgo-focus-text")
		     ((kbd "+") "meta C-+")
		     ((kbd "-") "meta C--")
		     ((kbd "0") "meta C-0")

		     ((kbd "b") "meta C-[")
		     ((kbd "f") "meta C-]"))))
  ;; hyper rat
  ((kbd "M-r") "ratsnap")
  ((kbd "H-j") "ratsnap left 75")
  ((kbd "H-k") "ratsnap down 75")
  ((kbd "H-l") "ratsnap right 75")
  ((kbd "H-i") "ratsnap up 75")
  ((kbd "H-s-j") "ratsnap left 20")
  ((kbd "H-s-k") "ratsnap down 20")
  ((kbd "H-s-l") "ratsnap right 20")
  ((kbd "H-s-i") "ratsnap up 20")
  ((kbd "H-SPC") "ratclick 1")
  ((kbd "H-s-SPC") "ratclick 2")
  ((kbd "s-SPC") "ratclick 3"))

(simkey:define-key-translation "Riot"
  ((kbd "C-f") "meta Right")
  ((kbd "C-b") "meta Left")
  ((kbd "C-a") "meta Home")
  ((kbd "C-e") "meta End")
  ((kbd "M-b") "meta C-Left")
  ((kbd "M-f") "meta C-Right")
  ((kbd "M-DEL") "meta C-DEL"))

(simkey:define-key-translation "calibre"
  ((kbd "C-p") "meta Up")
  ((kbd "C-n") "meta Down"))
