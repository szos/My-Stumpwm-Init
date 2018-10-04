;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;Defining Commands;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :stumpwm)

(defmacro cond-let (vars &body cond-body)
  `(let ,vars
     (cond ,@cond-body)))

(defun run-raise-or-pull (cmd props)
  "This function grabs a window based on props (and automatically list if there
are multiple matching windows) and does one of three things: if the window 
doesnt exist, it evaluates cmd. if the window is currently visible OR
is in a different group, it raises the window. otherwise the window is pulled
to the current frame. It is designed to take advantage of fuzzy-finder's 
multiple possible parameter searches, with an example call looking like:
\(run-raise-or-pull \"command\" '\(:class \"cc\" \)\)  OR
\(run-raise-or-pull \"command\" '\(\(:title \"title\"\)
                                   \(:class \"CC\"\)\) "
  (cond-let ((win (fuzzy-finder (if (listp (car props))
				    props
				    `(,props)))))
    ((not win)
     (if (stringp cmd)
	 (run-shell-command cmd)
	 (eval cmd)))
    ((or (window-visible-p win) (not (eq (window-group win) (current-group))))
     (raise win))
    (t
     (pull win))))

;; define some stuff to send keys to firefox. then add it to a change-window-hook 
;; basically try to emulate EXWM's exwm-input-set-simulation-keys

;;define functions:
(defun run-raise-or-list (cmd props &optional (all-groups *run-or-raise-all-groups*)
    				      (all-screens *run-or-raise-all-screens*)) 
      "Run the shell command, {cmd}, unless an existing window 
matches the {props} as described in run-or-raise. If {props} have 
multiple matches, generate a window list"
      (labels
          ;; Raise the window win and select its frame.  For now, it
          ;; does not select the screen.
          ((goto-win (win)
             (let* ((group (window-group win))
                    (frame (window-frame win))
                    (old-frame (tile-group-current-frame group)))
               (focus-all win)
               (unless (eq frame old-frame)
                 (show-frame-indicator group))))))
        (let ((matches (find-matching-windows props all-groups all-screens)))
          (case (length matches)
    	    (0 (run-shell-command cmd))
    	    (1 (if (eq (type-of (window-group (car matches))) 'float-group)
    		   (focus-all (car matches))
    		   (goto-win (car matches))))
    	    (t ;; (windowlist *window-format* matches)
	     (windowlist "%n%s%i => %t" matches)))))

(defun run-raise-list (props &optional (all-groups *run-or-raise-all-groups*)
    				      (all-screens *run-or-raise-all-screens*))
  (let ((matches (find-matching-windows props all-groups all-screens)))
    matches))

(defun custom-flatten (l)
  (if l
      (if (atom l)
	  (list l)
	  (mapcan #'custom-flatten l))))

;; (defcommand test-r-r-l-i () ()
;;   (run-raise-list-instances '((:class "Emacs") (:class "Firefox") (:class "Icecat"))))

(defun run-raise-list-instances (props &optional (all-groups *run-or-raise-all-groups*)
					 (all-screens *run-or-raise-all-screens*))
  (windowlist "%n%s%c => %t" (custom-flatten (loop for x in props
						collect (run-raise-list x all-groups all-screens)))))

(defcommand my-pull-menu () ()
  (labels ((pick-and-pull (windows)
	     (let ((selection (select-from-menu (current-screen) windows "")))
	       (pull-window selection))))
    (let ((corral (custom-flatten (loop for x in props
						collect (run-raise-list x all-groups all-screens)))))
      (pick-and-pull corral))))

;; make a new windowlist that just returns a window, nothing more. 

;;;; redefine everything above, refactored of course.
;;; THE NAME run-prog CONFLICTS WITH THE STUMPWM INTERNAL FUNCTION RUN-PROG.

;; (defun run-or-list (cmd props &optional (all-groups *run-or-raise-all-groups*)
;;     				      (all-screens *run-or-raise-all-screens*))
;;   "run props through fuzzy finder, if ")

(defun run-or-raise-or-list (cmd props &optional (all-groups *run-or-raise-all-groups*)
    				      (all-screens *run-or-raise-all-screens*))
  "see if the props bring anything, if not run cmd."
  (if-let ((match (fuzzy-finder props *window-format* all-groups all-screens)))
    (focus-all match)
    (run-shell-command cmd)))

(defun run-or-pull-or-list (cmd props &optional (all-groups *run-or-raise-all-groups*)
    				     (all-screens *run-or-raise-all-screens*))
  "see if the props bring anything, if not run cmd."
  (if-let ((match (fuzzy-finder props *window-format* all-groups all-screens)))
    (pull match)
    (run-shell-command cmd)))

(defmacro run-raise (cmd class)
  `(if-let ((win (fuzzy-finder '((:class ,class)))))
     (raise win)
     (run-shell-command ,cmd)))


(defun run-pull (cmd class)
  (if-let ((win (fuzzy-finder `((:class ,class)))))
    (pull win)
    (run-shell-command cmd)))
;; end

(defcommand all-pullall () ()
  (let* ((*window-format* "%n%s%c => %30t")
	 (win (fuzzy-finder)))
    (when win (pull win))))

(defcommand pullstr (str) ((:string "pull: "))
  (let* ((*window-format* "%n%s%c => %30t")
	 (win (fuzzy-finder `((:class ,str)))))
    (when win (pull win))))

(define-interactive-keymap gnext-map (:on-enter #'gnext) 
  ((kbd "'") "gnext");cycle groups
  ((kbd "C-'") "gnext"))

;; end functions

;; set up local dynamic vars for commands
;; tracks the status of the mode line to ensure stumptray is safely enabled and disabled. 
;; mode line is off --- 0
;; mode line is on ---- 1

;;; command to list fonts in xtf::*font-cache*
(defcommand list-ttf-fonts () ()
  "list all fonts in xtf::*font-cache*"
  (let ((list-font-keys nil)
	(4-to nil)
	(formatter ""))
    (maphash #'(lambda (key value)
		 (setf list-font-keys (cons key list-font-keys))
		 ;;(format t "Font: ~S |||| ~S~%" key value)
		 )
	     xft::*font-cache*)
    (mapcar #'(lambda (value)
		(setf formatter 
		      (concatenate 'string formatter value ;'(#\Newline)
				   )))
	    list-font-keys)
    ;(setf formatter (mapcar (format t "Font: ~S~%")))
    (setf 4-to (4-to-a-row list-font-keys))
    ;; (setf 4-to (flatten 4-to))
    (message "~S" list-font-keys)))

(defun 4-to-a-row (list &optional (pre "Fonts: "))
  (unless list pre)
  (cond ((fourth list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list) ", "
				  (third list) ", " (fourth list) '(#\Newline))))
	   (pop list) (pop list) (pop list) (pop list)
	   ;; (concatenate 'string hold (4-to-a-row (list)))
	   hold))
	((third list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list) ", "
				  (third list) '(#\Newline))))
	   (pop list) (pop list) (pop list)
	   hold))
	((second list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list)
				  '(#\Newline))))
	   (pop list) (pop list)
	   hold))
	(t
	 (let ((hold (concatenate 'string pre (first list) ". ")))
	   (pop list)
	   hold))))


(defcommand balanced-remove-split () ()
  (let ((win (current-window)))
    (remove-split)
    (balance-frames)
    (raise win)))

(defcommand frames-tester () ()
  (let ((frames (group-frames (current-group))))
    (message "~S~%~%~S" (cadr frames) (length frames))))

(defun set-sloppy ()
  (setf *mouse-focus-policy* :sloppy))

(defun set-ignore ()
  (setf *mouse-focus-policy* :ignore))

(defcommand toggle-mouse-mode () ()
  (if (equal *mouse-focus-policy* :ignore)
      (set-sloppy)
      (set-ignore)))

;;; Set up commands to grab floating windows that I've generated with
;;; with-open-window and float-in-tiles. its required for them to be listed.
(defcommand access-floats () ()
  "looks for windows floated with the (with-open-window... #'float-in-tiles)"
  (when-let ((win (fuzzy-finder '((:class "|FLOAT|")) *window-format* nil nil)))
    (eval (cadr (select-from-menu (current-screen)
				  `(("raise" (raise ,win))
				    ("focus" (focus-all ,win))
				    ("delete" (delete-window ,win))))))))

(defcommand access-floats-global () ()
  "looks for windows floated with the (with-open-window... #'float-in-tiles) 
based on users global settings"
  (when-let ((win (fuzzy-finder '((:class "|FLOAT|")) *window-format* t t)))
    (eval (cadr (select-from-menu (current-screen)
				  `(("raise" (raise ,win))
				    ("focus" (focus-all ,win))
				    ("delete" (delete-window ,win))))))))
;;; Load Application Commands.
(load "~/.stumpwm.d/commands-applications.lisp")

(defcommand waterfox-f () ()
  (with-open-window "waterfox" nil #'float-window (current-group)))

(defcommand youtube () ()
  "raise the firefox window open to youtube, or open a new window/tab open to youtube"
  (run-raise-or-pull "waterfox https://youtube.com" '((:title "YouTube"))))


(defparameter *kill-test* nil)

(defcommand watch-youtube-video (url) ((:string "URL:  "))
  (let ((x (frame-x (window-frame (current-window))))
	(y (frame-y (window-frame (current-window))))
	(w (frame-width (window-frame (current-window))))
	(h (frame-height (window-frame (current-window)))))
    (with-open-window  (format nil "mpv ~a" url) "mpv"
		       #'(lambda (cwin)
			   (float-in-tiles cwin :always-on-top t
			   		   :new-class "Youtube" :x x :y (if (< y 40)
			   							  37
			   							  y)
			   		   :width (- w 5) :height (if (< y 40)
			   					(- h 37)
			   					h))
			   (setf *kill-test* cwin)))))

(defcommand kill-yt () ()
  (kill-window *kill-test*))

(defcommand icecat () ()
  "icarun raise or list icecat"
  (run-or-raise-with-win "~/icecat/icecat" "Icecat" )
  (run-raise-or-list "~/icecat/icecat" '(:class "Icecat")))

(defun run-or-raise-with-win (cmd str &optional (function nil) &key (args nil) (restrictor nil))
  (if-let ((win (fuzzy-finder `((:class ,str)))))
    (raise win)
    (with-open-window cmd restrictor function args)))

(defcommand icecat-p () ()
  "icarun raise or list icecat"
  (run-raise-or-list "~/icecat/icecat --private-window" '(:class "Icecat")))

(defcommand icecat-n () ()
  "create new instance of portable icecat"
  (run-shell-command "~/icecat/icecat" ))

(defcommand birdfont () ()
  (run-shell-command "flatpak run org.birdfont.BirdFont/x86_64/master"))

(defcommand elinks () ()
  "runs an instance of elinks from the terminal"
  (run-shell-command "xfce4-terminal -e elinks"))
(defcommand elinks-anon () ()
  "runs instance of elinks without connecting to the head elinks process and connects anonymously"
  (run-shell-command "cool-retro-term -e elinks -anonymous 1 -no-connect 1"))

(defcommand list-browsers () ()
  "raise a list of all the browsers open"
  (fuzzy-finder '((:class "Firefox") (:class "Icecat") (:class "Conkeror"))))

;;; specific webpages (using fuzzy-finder)
;; (defcommand youtube () ()
;;   "find YouTube in the title field."
;;   (find-then-do "YouTube" "title"))



(defcommand wpull (search) ((:string "Class to Pull: "))
  (if-let ((win (fuzzy-finder `((:class ,search)))))
    (pull win)
    (message "No Matching Windows Found")))

;;; Media


;;; end media


(defcommand deluge () ()
  (run-raise-or-pull "deluge" '(:class "Torrent Client")))

(defun xterm (&optional (args nil)) 
  (run-raise-or-pull (if args
			 (format nil "xterm ~a" args)
			 "xterm") '(:class "XTerm")))

(defun xterm-new (&optional (args nil))
  (run-shell-command (if args
			 (format nil "xterm ~a" args)
			 "xterm")))

(defcommand terminal () ()
  (run-raise-or-pull "cool-retro-term" '(:class "cool-retro-term")))

(defcommand terminal-new () ()
  (run-shell-command "cool-retro-term"))

(defcommand termacs () ()
  (with-open-window "cool-retro-term -e emacs -nw" "cool-retro-term"
		    #'(lambda (cwin)
			(reclassify-window cwin "Emacs")
			;; (meta "M-x")
			)))

(defcommand term () ()
  (xterm))

(defcommand term-new () ()
  (xterm-new))

(defcommand shcl () ()
  "launch a shell written in common lisp"
  (run-shell-command "xfce4-terminal -e /home/shos/LISP/shcl/shcl"))

(defcommand newsboat () ()
  (run-raise-or-pull
   '(with-open-window "xterm -e newsboat" "XTerm"
     #'reclassify-window "Newsboat")
   '(:class "Newsboat")))

(defcommand kdenlive () ()
  (run-raise-or-list "kdenlive" '(:class "Video Editor")))

(defcommand temp-sensors () ()
  (run-or-raise-or-list "xfce4-sensors" '((:class "Xfce4-sensors"))))

(defcommand menu () () ;; this isnt working for some reason
  (run-raise-or-list "menulibre" '(:class "Menu")))

;;;; begin general definitions taken from menulibre
;;; XFCE definitions
(defcommand log-out () () 
  (run-or-raise-or-list "xfce4-session-logout" '((:class "Xfce4-session-logout"))))
(defcommand appfinder () () 
  (run-raise-or-pull "xfce4-appfinder" '((:class "Xfce4-appfinder"))))

(defcommand screenshot () ()
  (with-open-window "xfce4-screenshooter" nil #'float-window (current-group)))

(defcommand task-manager () () 
  (run-raise-or-pull "xfce4-taskmanager" '(:class "Task Manager")))
(defcommand xfce4-terminal () ()
  "run xfce terminal"
  (run-raise-or-pull "xfce4-terminal" '(:class "Terminal")))
(defcommand pamac () () 
  (run-raise-or-list "pamac-manager" '(:class "Software Manager")))
(defcommand power-manager () ()
  (run-raise-or-list "xfce4-power-manager-settings" '(:class "Power Manager")))
;;; XFCE end

(defcommand calculator () () 
  (run-raise-or-list "galculator" '(:class "Utility")))
(defcommand keepass () () 
  (run-raise-or-list "keepass" '(:class "Utility")))

(defcommand lem () ()
  (run-raise-or-pull '(with-open-window "xterm -e ./.roswell/bin/lem" "XTerm"
  		       #'(lambda (cwin)
  			   (setf (window-class cwin) "Lem")))
		     '(:class "Lem")))

(defcommand mousepad () ()
  ;;; (with-open-window "mousepad" nil #'float-in-tiles)
  (run-shell-command "mousepad"))
(defcommand mouse-notes () ()
  (with-open-window '("mousepad" "vsplit") nil #'(lambda (cwin)
						   (float-in-tiles cwin
								   :new-class
								   "mouse-notes"
								   :width 540
								   :height 400))))
(defcommand slimeball () ()
  (if-let ((win (fuzzy-finder '((:class "|FLOAT|Slimeball")))))
    (if (eq (window-group win) (current-group))
	(raise win)
	(eval (second (select-from-menu (current-screen)
					`(("Jump to Slimeball" (raise ,win))
					  ("Stay Here" nil))))))
    (with-open-window "emacs" nil
		      #'(lambda (cwin)
			  (float-in-tiles cwin
					  :new-class "Slimeball"
					  :width 540
					  :height 400
					  :x 10
					  :y 70)
			  (multi-meta '("M-x" "menu-bar-mode" "RET" "M-x"
					"slime-connect" "RET" "RET" "DEL"
					"6" "RET")
				      cwin)
			  (run-with-timer 3 nil
					  #'(lambda ()
					      (multi-meta
					       '("(in-package :stumpwm)" "RET"
						 "C-x" "1")
					       cwin)))))))

(defcommand replball () ()
  (if-let ((win (fuzzy-finder '((:class "|FLOAT|Replball")))))
    (if (eq (window-group win) (current-group))
	(raise win)
	(eval (second (select-from-menu (current-screen)
					`(("Jump to Replball" (raise ,win))
					  ("Stay Here" nil))))))
    (with-open-window "cool-retro-term -e emacs -nw" "cool-retro-term"
		      #'(lambda (cwin)
			  (float-in-tiles cwin
					  :new-class "Replball"
					  :width 540
					  :height 400
					  :x 10
					  :y 70)
			  (multi-meta '("M-x" "menu-bar-mode" "RET" "M-x"
					"slime-connect" "RET" "RET" "DEL"
					"6" "RET")
				      cwin)
			  (run-with-timer 2 nil
					  #'(lambda ()
					      (multi-meta
					       '("(in-package :stumpwm)" "RET"
						 "C-x" "1")
					       cwin)))))))

(defcommand notes () ()
  (if-let ((win (fuzzy-finder '((:class "|FLOAT|Notes")))))
    (if (eq (window-group win) (current-group))
	(raise win)
	(eval (second (select-from-menu (current-screen) `(("Jump to Notes" (raise ,win))
							   ("Stay Here" nil))))))
    (with-open-window "emacs ~/.stumpwm.d/notes.org" nil
		      #'(lambda (cwin)
			  (float-in-tiles cwin
					  :new-class "Notes"
					  :width 540
					  :height 400
					  :x 10
					  :y 70)
			  (multi-meta '("M-x" "enable-notes" "RET"))))))

(defcommand mount-partition (partition mount-point password)
    ((:string "partition to mount:  ")
     (:string "where to mount it:  ")
     (:password "password:  "))
  (with-open-window "cool-retro-term" nil
		    #'(lambda (cwin)
			(multi-meta
			 `(,(concatenate 'string  "sudo mount "
					 partition " " mount-point)
			    "RET" ,password "RET")
			 cwin)
			(run-with-timer .5 nil #'meta (kbd "C-Q")))))

(define-condition kbd-parse ()
  ((text :initarg :text :reader text)))

(defun kbd? (string)
  "takes a string and returns the result of calling {kbd} 
with string, or if it isnt valid return nil."
  (handler-case (kbd string)
    (kbd-parse-error () nil)
    (kbd-parse () nil)))

(defun multi-meta (strings &optional (cwin (current-window)))
  "focuses the specified window, checks whether {kbd} errors out, and 
either sends the {kbd} result or "
  (unless (eq (current-window) cwin)
    (focus-all cwin))
  (if-let ((key (kbd? (car strings))))
    (meta key)
    (window-send-string (car strings)))
  (when (cdr strings)
    (multi-meta (cdr strings))))

(defcommand floating-resize (w h) ((:number "W: ")
				   (:number "H: "))
  (let* ((win (if (search "|FLOAT|" (window-class (current-window)))
		  (current-window)
		  (fuzzy-finder '((:class "|FLOAT|")))))
	 (old-x (window-x win))
	 (old-y (window-y win)))
    (float-window-move-resize win :x 0 :y 0 :width w :height h)
    (run-with-timer .1 nil #'(lambda ()
			       (float-window-move-resize win
							 :x old-x :y old-y
							 :width w :height h)))))

(defcommand portacle () ()
  (run-raise-or-pull
   '(with-open-window "sh /home/shos/LISP/portacle/portacle.run" "Emacs"
     #'reclassify-window "Portacle")
   '(:class "Portacle")))

;;;begin pacman and other system bits
(defcommand update-system (pwd) ((:password "PWD: "))
  (with-open-window "xfce4-terminal" "Xfce4-terminal"
		    #'(lambda (cwin);;sudo pacman -Syu
			(multi-meta `("sudo pacman -Syu" "RET" ,pwd "RET") cwin))))

(defcommand gparted () ()
  (run-shell-command "gparted"))

;; Games
(defcommand feed-the-beast () () 
  (run-raise-or-list "feedthebeast" '(:class "Game")))
(defcommand minecraft () () 
  (run-raise-or-list "minecraft" '(:class "Game")))
;; Games end

;; Graphics
(defcommand darktable () () 
    (run-raise-or-list "darktable" '(:class "Photo Editor")))
(defcommand gimp () () 
    (run-raise-or-list "gimp" '(:class "Photo Editor")))

(defmacro conde (&body body)
  (if (not body)
      nil
      `(let ((return-list (cons  (if ,(car body)
				     ,(cadr body)) 
				 (conde ,@(rest (rest body))))))
	 return-list)))

(defun current-window-set-by-key (&key (class nil) (title nil) (role nil) (window (current-window)))
  "takes key arguments which when provided set the window slot designated 
and assign it to the argument provided."
  (when class (setf (window-class window) class))
  (when title (setf (window-title window) title))
  (when role (setf (window-role window) role)))

(defcommand alsamixer () ()
  (with-open-window "cool-retro-term -e alsamixer" "cool-retro-term" #'reclassify-window "Alsamixer"))

(defcommand pulse-audio () ()
  (run-raise "pavucontrol" "Pavucontrol"))

(defcommand restart-pia () ()
  (with-open-window "cool-retro-term" "cool-retro-term"
		    #'(lambda (cwin)
			(multi-meta '("killall ruby"
				     "RET" "cd" "RET"
				     "./pia.sh" "RET")
				    cwin))))

(defun watch-video-ascii (pathname)
  (run-shell-command
   (format nil "xfce4-terminal -e CACA_DRIVER=ncurses mplayer -vo caca -quiet ~a" pathname)))

(defcommand watch-ascii-vid (file) ((:file "File Path:  "))
  (run-shell-command
   (format nil "xfce4-terminal -e CACA_DRIVER=ncurses mplayer -vo caca -quiet ~a" pathname)))

(defcommand calibre () ()
  (run-raise-or-pull "calibre" '(:class "calibre")))
