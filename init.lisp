;;; ;;; -*-lisp-*-
(in-package :stumpwm)
;; load dependencies
(ql:quickload :xembed)
(ql:quickload :stumpbuffer)
(ql:quickload :clx-truetype)
(ql:quickload :clx)
(ql:quickload :utilities)

;(load "/home/shos/.stumpwm.d/misc-macros.lisp")
;; ;; timeout wait.
(setf *timeout-wait* 15)

;; ;;set startup message
(setf *startup-message* (concatenate 'string "SHOS@" (machine-instance) " | "
				     "Initializing"))

;; ;; put message window and input window in the center
(setf *message-window-gravity* :top)

;; ;; change the prefix key to something else
(set-prefix-key (kbd "C-;"))
;; (set-prefix-key (kbd "s"))
;; properly
(define-key *root-map* (kbd ";") "colon") ;; needed for some reason. 

;; ;;swap caps and ctrl
;; (run-shell-command "setxkbmap -option ctrl:swapcaps")

;; initialize system
(run-shell-command "xmodmap ~/.stumpwm.d/kcode.modmap")
(run-shell-command "xscreensaver")
(run-shell-command "/usr/lib/notification-daemon-1.0/notification-daemon")
(run-shell-command "/usr/bin/lxqt-policykit-agent")

(set-module-dir "/home/shos/.stumpwm.d/")
;;(add-to-load-path "~/.stumpwm.d/custom-modules/ratcontrol/")
;;(add-to-load-path "~/.stumpwm.d/custom-modules/translation-keys/")
;;(add-to-load-path "~/.stumpwm.d/custom-modules/matrix-stump/")
;; (init-load-path "/home/shos/.stumpwm.d/translation-keys")
;; initialize our module directory. stumpwm defualts to 
;; (init-load-path "/home/shos/.stumpwm.d/contrib/")
(defcommand define-hydra (bindings)
    ((:rest "bindings: "))
  (let ((k (make-sparse-keymap)))
    (when  (stringp bindings)
      (setf bindings (read-from-string bindings)))
    (mapcar #'(lambda (binding)
		(funcall #'define-key k (eval (car binding)) (second binding)))
	    bindings)
    k))

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

;; (translation-keys:define-key-translations "Firefox"
;;     (("C-g" "meta ESC")
;;      ("C-q" "meta SunPageDown")
;;      ("C-v" "meta SunPageDown")
;;      ("M-v" "meta SunPageUp")
;;      ("C-y" "meta C-v")
;;      ("M-w" "meta C-c")
;;      ("C-w" "meta C-x")
;;      ("C-s" "meta C-f")
;;      ("C-r" "meta C-S-g")
;;      ("C-n" "meta Down")
;;      ("C-p" "meta Up")
;;      ("C-f" "meta Right")
;;      ("C-b" "meta Left")
;;      ("C-B" "meta C-[")
;;      ("C-F" "meta C-]")
;;      ("M-f" "meta C-t")n
;;      ("M-b" "meta C-S-t")
;;      ("M-<" "meta Home")
;;      ("M->" "meta End")
;;      ("M-s" "meta C-l")
;;      ("s-f" "meta '")))

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
	     ("this is a hydra, defining k as quit, etc..."))))

;; begin loading modules:
(load-module "stumptray")
(load-module "ratcontrol")
(load-module "app-menu")
(app-menu:load-menu-file "/home/shos/.stumpwm.d/appmenu-search.lisp")
(app-menu:load-menu-file "/home/shos/.stumpwm.d/appmenu.lisp")
(app-menu:load-menu-file-sys "/home/shos/.stumpwm.d/system-menu.lisp")

(load-module "end-session")
;; (load-module "power") ;; not working due to iolib failing, and thus preventing dbus from installing
(load-module "net")
(load-module "disk")
(load-module "mem")
(load-module "cpu")
(load-module "hostname")
(load-module "battery-portable")
(load-module "notifications")
(load-module "windowtags")
(load-module "globalwindows")
;;; load fonts, needed after latest update (1 june 2018)
(load-module "ttf-fonts")
;;;; various font examples, you can check the available fonts by formatting 
;;;; with the function print-hash-table and the hash table *font-cache*
;; (set-font (make-instance 'xft:font :family "Droid Sans Mono" :subfamily "Regular" :size 11))
;; (set-font (make-instance 'xft:font :family "malayalam" :subfamily "Regular" :size 11))
(set-font (make-instance 'xft:font :family "Bitstream Vera Sans Mono" :subfamily "Roman" :size 10))
;; (set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
;; (set-font "ter-u<6x12><n>.bdf")
;; (set-font "-misc-cantarell-medium-r-normal--0-0-0-0-p-0-iso8859-15")

;; (load-module "clipboard-history")
;; end loading modules.
;; Initialize modules
;; (app-menu:load-menu-file "/home/shos/.stumpwm.d/appmenu-pinned.lisp")

(setf *mode-line-background-color* "#1f1f1f")
(setf *mode-line-border-color* (seventh *colors*))

(setf *screen-mode-line-format*
      (list "^6^B%B^b | %c%t %f | %l|[%M] %N| Room: "
	    ;;'(:eval (room nil))
	    "Volume: " 
	    '(:eval *volume-readout*)
	    '(:eval *volume-overdrive*)
	    "[===^3===^1=*^7---]^6 %d
"
	    ;; crashes stump!'(:eval (run-shell-command "amixer get Master"))
	    ;; '(:eval (run-shell-command "date +%T" t))
      	    "%h | %g | %W"))

;; (defvar *battery-status-command*
;;   "acpi -b | awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' | sed s/Discharging/\-/ | sed s/Unknown// | sed s/Full// | sed s/Charging/+/")

;; (defvar *vol-status-command*
;;   "amixer get Master | grep [[:digit:]]\\+%' -o | tr -d '\\n'")

(setf *mode-line-timeout* 1)

;; set input/message bar

(set-fg-color (caddr (cddddr *colors*)))
(set-border-color (caddr (cddddr *colors*)))
(setf *input-window-gravity* :top)


;; (clipboard-history:start-clipboard-manager)
;; End initialization of modules

;; define global that can printout battery status

;; (defvar *battery-status-command*
;;   "acpi -b | awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' | sed s/Discharging/\-/ | sed s/Unknown// | sed s/Full// | sed s/Charging/+/")

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.n

;; (defcommand colon1 (&optional (initial "")) (:rest)
;;   (let ((cmd (read-one-line (current-screen) "> " :initial-input initial)))
;;     (when cmd
;;       (eval-command cmd t))))

;; (defun colon-test (win &optional (initial ""))
;;   (let ((cmd (read-one-line (current-screen) "> " :initial-input initial)))
;;     (push cmd win)
;;     (message "~S" win;; ;;     ))
;; (defmacro test-colon (win)
;;   `(let ((cmd (read-one-line (current-screen) "> ")))
;;      (funcall ,cmd ,win)))

;; (test-colon (fuzzy-finder '((:class "Firefox"))))
  

;; load files
(load "~/.stumpwm.d/interactive-maps.lisp")
(load "~/.stumpwm.d/with-open-window.lisp")
(load "~/.stumpwm.d/commands-reclass.lisp")
(load "~/.stumpwm.d/windows-groups-frames.lisp")
(load "~/.stumpwm.d/commands.lisp")
;; (load "~/.stumpwm.d/ratcontrol.lisp")
(load "~/.stumpwm.d/expose.lisp")
;; (load "~/.stumpwm.d/test.lisp")
(load "~/.stumpwm.d/commands-utilities.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/floats-in-tiles.lisp")

;; (load "~/.stumpwm.d/scratch.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Web jump (works for Google and Imdb)
;; (defmacro make-web-jump (name prefix)
;;   `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
;;     (substitute #\+ #\Space search)
;;     (run-shell-command (concatenate 'string ,prefix search))))

;; (make-web-jump "google" "firefox https://www.google.com/search?q=")
;; (make-web-jump "imdb" "firefox https://www.imdb.com/find?q=")
;; (make-web-jump "duckduckgo" "firefox http://www.duckduckgo.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
;; (define-key *root-map* (kbd "M-s") "google")
;; (define-key *root-map* (kbd "i") "imdb")

;; Message window font

;; Define window placement policy...

;; Clear rules
(clear-window-placement-rules)

;; ;; Last rule to match takes precedence!
;; ;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; ;; match is performed.
;; ;; TIP: if the :create flag is set then a missing group will be created and
;; ;; restored from *data-dir*/create file.
;; ;; TIP: if the :restore flag is set then group dump is restored even for an
;; ;; existing group using *data-dir*/restore file.
;; (define-frame-preference "Home"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;   (1 t nil :class "XTerm"))

;; (define-frame-preference "Ardour"
;;   (0 t   t   :instance "ardour_editor" :type :normal)
;;   (0 t   t   :title "Ardour - Session Control")
;;   (0 nil nil :class "XTerm")
;;   (1 t   nil :type :normal)
;;   (1 t   t   :instance "ardour_mixer")
;;   (2 t   t   :instance "jvmetro")
;;   (1 t   t   :instance "qjackctl")
;;   (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

;; (define-frame-preference "Shareland"
;;   (0 t   nil :class "XTerm")
;;   (1 nil t   :class "aMule"))

;; (define-frame-preference "Dev"
;;   (1 t t :restore "emacs-editing-dump" :title "...xdvi")
;;   (0 t t :create "emacs-dump" :class "Emacs"))


;; ;;; beginning user defined bits and bobs
;; ;;make mouse click focus window
;; ;; (setf *mouse-focus-policy* :click)
;; ;;; finish setting up:
;; ;; run xkeysnail
;; ;; (run-shell-command "sudo xkeysnail ~/xkeysnail-master/config.py")
;; ;; STUMPTRAY!

;; (clear-window-placement-rules)

;; ;; Set up Volume control and messaging
;; ;; testing;;;;

(defun renumber-windows (arg)
  "this needs to be a function which takes an arg. I just throw out the arg.
 --experiment: message the argument!"
  (stumpwm:repack-window-numbers)
  (message "Closed: ~S" (window-title arg)))

(add-hook *destroy-window-hook* 'renumber-windows)

;;; this below may be needed? 

(defun slime-package? (package-name)	
  (equal (subseq package-name 0 5) "slime"))

(defun get-slime-package-path ()
  (let* ((elpa-path (namestring
                     (merge-pathnames ".emacs.d/elpa/"
                                      (user-homedir-pathname))))
         (pkg-version (car
                       (remove-if-not
                        #'slime-package?
                        (split-string
                         (run-shell-command
                          (concatenate 'string "ls " elpa-path) t)))))
         (slime-path (merge-pathnames pkg-version elpa-path)))
    (concatenate 'string (namestring slime-path) "/")))

(load (merge-pathnames "swank-loader.lisp" (get-slime-package-path)))

;;; end maybe needed bit

;; (ql:quickload :swank)
;; load swank to connect via emacs
(require :swank)
(swank-loader:init)
(swank:stop-server 4006)
(swank:create-server :port 4006
		     :style swank:*communication-style*
		     :dont-close t)



