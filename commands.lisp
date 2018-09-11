;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;Defining Commands;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :stumpwm)

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
;;; browsers
(defcommand firefox () ()
  "run firefox or set focus to it f already running"
  (run-or-raise-or-list "firefox" '((:class "Firefox"))))
(defcommand firefox-n () ()
  "run firefox"
  (run-shell-command "firefox"))
    
(defcommand waterfox () ()
  "Run or raise or list waterfox"
  (run-or-raise-or-list "waterfox" '((:class "Waterfox"))))

(defcommand waterfox-n () ()
  "run waterfox"
  (run-shell-command "waterfox"))

(defcommand waterfox-p () ()
  "run private window"
  (run-shell-command "waterfox --private-window"))

(defcommand waterfox-f () ()
  (with-open-window "waterfox" nil #'float-window (current-group)))

(defun wfx (&optional (args nil))
  (if (not args)
      (run-shell-command "waterfox")
      (run-shell-command (concatenate 'string "waterfox " args))))

(defcommand youtube () ()
  "raise the firefox window open to youtube, or open a new window/tab open to youtube"
  (run-or-raise-or-list "waterfox https://youtube.com" '((:title "YouTube"))))

(defcommand conkeror () ()
  "run conkeror or raise it"
  (run-raise-or-list "conkeror" '(:class "Conkeror")))
(defcommand conkeror-n () ()
  "run a new conkeror instance"
  (run-shell-command "conkeror"))

(defcommand tor () ()
  "runs or raises tor."
  (if-let ((win (fuzzy-finder '((:class "Tor Browser")))))
    (raise win)
    (run-shell-command "./TOR/Browser/start-tor-browser")))

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

(defcommand w3m () ()
  "runs w3m open to duckduckgo"
  (run-shell-command "cool-retro-term -e w3m duckduckgo.com"))

(defcommand elinks () ()
  "runs an instance of elinks from the terminal"
  (run-shell-command "xfce4-terminal -e elinks"))
(defcommand elinks-anon () ()
  "runs instance of elinks without connecting to the head elinks process and connects anonymously"
  (run-shell-command "cool-retro-term -e elinks -anonymous 1 -no-connect 1"))

(defcommand list-browsers () ()
  "raise a list of all the browsers open"
  (fuzzy-finder '((:class "Firefox") (:class "Icecat") (:class "Conkeror"))))

(defcommand riot () ()
	    (exec riot-desktop))

;;; specific webpages (using fuzzy-finder)
;; (defcommand youtube () ()
;;   "find YouTube in the title field."
;;   (find-then-do "YouTube" "title"))

(defprogram-shortcut thunderbird)

(defcommand mail () ()
  (run-or-raise-or-list "thunderbird" '((:class "Thunderbird"))))

(defcommand qtox () ()
  (run-or-raise-or-list "qtox" '((:class "qTox"))))

(defcommand Riot () ()
  (run-raise "riot-desktop" "Riot"))

;;; Media
(defcommand vlc () ()
  (run-or-raise-or-list "vlc" '((:class "vlc"))))

(defcommand mpv-minimal () ()
  "run mpv with the pseudo-gui frontend"
  (run-or-raise-or-list "mpv --player-operation-mode=pseudo-gui" '((:class "mpv"))))

(defcommand mpv-baka () ()
  "run mpv with the baka frontend"
  (run-or-raise-or-list "baka-mplayer" '((:class "baka-mplayer"))))
;;; end media

(defprogram-shortcut thunar)

(defcommand file-manager () ()
  "runs your file manager in the current buffer"
  (run-raise-or-list "thunar" '(:class "File Manager")))

(defcommand deluge () ()
  (run-raise-or-list "deluge" '(:class "Torrent Client")))

(defcommand term () ()
  (run-raise "cool-retro-term" "cool-retro-term"))

(defcommand term-new () ()
  (run-shell-command "cool-retro-term"))

(defcommand termacs () ()
  (with-open-window "cool-retro-term -e emacs -nw" "cool-retro-term"
		    #'(lambda (cwin)
			(reclassify-window cwin "Emacs")
			;; (meta "M-x")
			)))

(defcommand terminal () ()
  (xfce4-terminal))

(defcommand shcl () ()
  "launch a shell written in common lisp"
  (run-shell-command "xfce4-terminal -e /home/shos/LISP/shcl/shcl"))

(defcommand kdenlive () ()
  (run-raise-or-list "kdenlive" '(:class "Video Editor")))

(defcommand temp-sensors () ()
  (run-or-raise-or-list "xfce4-sensors" '((:class "Xfce4-sensors")))
  (run-with-timer .5 nil 'float-this))

(defcommand menu () () ;; this isnt working for some reason
  (run-raise-or-list "menulibre" '(:class "Menu")))

;;;; begin general definitions taken from menulibre
;;; XFCE definitions
(defcommand log-out () () 
  (run-or-raise-or-list "xfce4-session-logout" '((:class "Xfce4-session-logout"))))
(defcommand appfinder () () 
  (run-or-raise-or-list "xfce4-appfinder" '((:class "Xfce4-appfinder"))))

(defcommand screenshot () ()
  (with-open-window "xfce4-screenshooter" nil #'float-window (current-group)))

(defcommand task-manager () () 
  (run-raise-or-list "xfce4-taskmanager" '(:class "Task Manager")))
(defcommand xfce4-terminal () ()
  "run xfce terminal"
  (run-raise-or-list "xfce4-terminal" '(:class "Terminal")))
(defcommand pamac () () 
    (run-raise-or-list "pamac-manager" '(:class "Software Manager")))
(defcommand power-manager () ()
  (run-raise-or-list "xfce4-power-manager-settings" '(:class "Power Manager")))
;;; XFCE end

(defcommand calculator () () 
  (run-raise-or-list "galculator" '(:class "Utility")))
(defcommand keepass () () 
  (run-raise-or-list "keepass" '(:class "Utility")))
(defcommand lightdm-settings () () 
  (run-raise-or-list "lightdm-gtk-greeter-settings-pkexec" '(:class "Setting")))
(defcommand mousepad () ()
  ;;; (with-open-window "mousepad" nil #'float-in-tiles)
  (run-shell-command "mousepad"))
(defcommand mouse-notes () ()
  "this doesnt work!!"
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
			  (run-with-timer 1 nil
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

(defcommand mount-data (pwd) ((:password "password: "))
  (with-open-window "cool-retro-term" nil
		    #'(lambda (cwin)
			(multi-meta `("sudo mount /dev/sda3 /home/shos/Data"
				      "RET" ,pwd "RET")
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

(defcommand portacle-instance () ()
  (with-open-window "sh /home/shos/LISP/portacle/portacle.run" "Emacs" #'reclassify-window "Portacle"))

;;;begin pacman and other system bits
( defcommand update-system () ()
  (run-shell-command "xfce4-terminal -e sudo pacman -Syu"))

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
  (with-open-window "cool-retro-term -e alsamixer" "cool-retro-term" #'reclassify-window "Alsamixer")
  ;; (run-with-timer .5 nil 'rename-current-windows-class "cool-retro-term" "AlsaMixer")
)
