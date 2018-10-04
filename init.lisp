;;; ;;; -*-lisp-*-
(in-package :stumpwm)
;; load dependencies
(ql:quickload :xembed)
(ql:quickload :stumpbuffer)
(ql:quickload :clx-truetype)
(ql:quickload :clx)
(ql:quickload :utilities)

(defmacro λ (args &body body)
  "This is just a small wrapper around lambda to make it easier to write.
This function works with λ or Λ. However, some fonts dont support λ and Λ.
additionally, clx doesnt support λ, so these cant be used in stumpwm commands"
  `(lambda ,args ,@body))

(defmacro rλ (args &body body)
  "this is a recursive anonymous function. we can 
call it from within the body with '(recurse args)'"
  `(lambda ,args
     (labels ((recurse ,args
		,@body))
       (recurse ,@args))))

;(load "/home/shos/.stumpwm.d/misc-macros.lisp")
;; timeout wait.
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

;; (setf *mouse-focus-policy* :sloppy)

;; ;;swap caps and ctrl
;; (run-shell-command "setxkbmap -option ctrl:swapcaps")

;; initialize system
;;; this is for an alt green enabled english modmap for more modifiers and keys!
(run-shell-command "setxkbmap no")
(run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-no.modmap")
(defparameter *xmodmaps*
  (let ((x nil))
    (lambda ()
      (if (not x)
	  (progn
	    (setf x t)
	    (run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-altgrn.modmap")
	    (message "xmodmap set to eng-altgrn.modmap"))
	  (progn
	    (setf x nil)
	    (run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-no.modmap")
	    (message "xmodmap set to eng-no.modmap"))))))

(defcommand change-layout () ()
  (funcall *xmodmaps*))

(run-shell-command "xscreensaver")
;; (run-shell-command "xfce4-power-manager")
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
      (list "^6^B%B^b | %c%t %f | %l|[%M] %N| "
	    ;;'(:eval (room nil))
	    '(:eval *volume-percentage*)
	    '(:eval *brightness-mode-line*)
	    " %d
"
	    ;; crashes stump!'(:eval (run-shell-command "amixer get Master"))
	    ;;  '(:eval (run-shell-command "date +%T" t))
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
;; (load "~/.stumpwm.d/commands-reclass.lisp")
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
;; ;; 
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
(handler-case (swank:create-server :port 4006
			      :style swank:*communication-style*
			      :dont-close t)
  (sb-bsd-sockets:address-in-use-error () (message "Swank Already Running")))

;;; heres some extra key translations:

(defun send-fake-extra-key (win key)
  "Send a fake key press event to win."
  (let ((xwin (window-xwin win)))
    (multiple-value-bind (code state) (grab-code-test key)
      (dolist (event '(:key-press :key-release))
        (xlib:send-event xwin
                         event
                         (xlib:make-event-mask event)
                         :display *display*
                         :root (screen-root (window-screen win))
                         ;; Apparently we need these in here, though they
                         ;; make no sense for a key event.
                         :x 0 :y 0 :root-x 0 :root-y 0
                         :window xwin
                         :event-window xwin
                         :code code
                         :state state)))))


;; (meta-intl "Ø")

(defun grab-code-test (key-str)
  (cond ((string= key-str "Ø")
	 (values 47 0))
	(t (key-to-keycode+state key-str))))

;; (meta (kbd "ø"))


;; (defparameter *OE-key* (kbd "Ø"))
;; (defparameter *oe-key* (kbd "ø"))
;; (defparameter *AE-key* (kbd "Æ"))
;; (defparameter *ae-key* (kbd "æ"))
;; (defparameter *aa-key* (kbd "å"))
;; (defparameter *AA-key* (kbd "Å"))
;; (defparameter *test-k2* (kbd "E"))

(defun key-to-keycode+state-backup (key)
  (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
    (cond ((eq (xlib:keycode->keysym *display* code 0) (key-keysym key))
           (values code (x11-mods key)))
          ((eq (xlib:keycode->keysym *display* code 1) (key-keysym key))
           (values code (apply 'xlib:make-state-mask
                               (cons :shift (xlib:make-state-keys (x11-mods key))))))
          (t
           ;; just warn them and go ahead as scheduled
           (warn "Don't know how to encode ~s" key)
           (values code (x11-mods key))))))
