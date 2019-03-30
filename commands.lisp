(in-package :stumpwm)

(defmacro cond-let (vars &body cond-body)
  `(let ,vars
     (cond ,@cond-body)))

(defun current-frame ()
  (let ((window-type (type-of (current-window))))
    (if (eq window-type 'float-window)
	nil ;; (focus-all (current-window))
	(window-frame (current-window)))))

;; (defun actually-switch-to-sibling ()
;;   (let ((sib (grab-sibling)))
;;     (if (listp sib)
;; 	(curframe)
;; 	(focus-frame (current-group) sib))))

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
  (let ((win (fuzzy-finder :props (if (listp (car props))
				    props
				    `(,props))
			   :all-groups nil)))
    (cond ((not win) ;; this means that fuzzy finder was quit by the user. 
	   (let ((opt (second (select-from-menu (current-screen) `(("RUN" ,cmd)
     								   ("EXIT" nil))))))
	     (when opt
	       (if (stringp opt)
     		   (run-shell-command opt)
     		   (eval opt)))))
	  ((equalp win :not-found)
	   (if (stringp cmd)
     	       (run-shell-command cmd)
     	       (eval cmd)))
	  ;; ((or (window-visible-p win) (not (eq (window-group win) (current-group))))
	  ;;  (raise win)) ;; this raises the window when it shouldnt. 
	  ((not (eq (window-group win) (current-group)))
	   (let ((choice (second (select-from-menu (current-screen) `(("pull" :pull)
								      ("raise" :raise)
								      ("quit" nil))))))
	     (cond ((eq choice :pull)
		    (pull win t))
		   ((eq choice :raise)
		    (raise win)))))
	  ((window-visible-p win)
	   (raise win))
	  (t
	   (pull win)))))

;;; keep screen from sleeping

(defparameter *jiggle-timer*
  nil
  "a timer for moving the mouse to keep the screen awake")

(defcommand enable-jiggle () ()
  "this is for keeping the screen awake, for example when watching a movie"
  (when (timer-p *jiggle-timer*)
    (cancel-timer *jiggle-timer*))
  (setf *jiggle-timer*
	(run-with-timer 0 100 #'(lambda ()
				  (ratrelwarp 1 1)
				  (ratrelwarp -1 -1))))
  (message "Cursor jiggle enabled"))

(defcommand disable-jiggle () ()
  "stops the cursor jiggle. "
  (when (timer-p *jiggle-timer*)
    (cancel-timer *jiggle-timer*)
    (message "Cursor jiggle disabled")))

;;; copy paste commands
(defcommand sendx () ()
  (window-send-string (get-x-selection)))
(defcommand send-string (str) ((:string "string to send: "))
  (window-send-string str))

;;; pull a window to the current group. 

(defcommand pull-to-group (str) ((:string "pull: "))
  (let* ((*window-format* "%n%s%c => %30t")
	 (win (fuzzy-finder :props `((:class ,str)) :groups-to-omit (list (current-group)))))
    (when (and win (not (equalp win :not-found)))
      (pull win))))

(defcommand test{echo} (strng) (:string "pull: ")
  (message "echoing ~S" strng))

(defcommand pull-from-global () ()
  (let ((*window-format* "%n%s%c => %30t"))
    ()))

;;;

(defcommand terminal () ()
  (run-raise-or-pull "cool-retro-term" '(:class "cool-retro-term")))

(defcommand terminal-new () ()
  (run-shell-command "cool-retro-term"))

(defun xterm (&optional (args nil)) 
  (run-raise-or-pull (if args
			 (format nil "xterm ~a" args)
			 "xterm")
		     '(:class "XTerm")))

(defun xterm-new (&optional (args nil))
  (run-shell-command (if args
			 (format nil "xterm ~a" args)
			 "xterm")))

(defcommand term () ()
  (xterm))

(defcommand term-new () ()
  (xterm-new))

(defcommand tmux (&optional (session-name "Main" session-name-provided-p)) ()
  ;; () ()
  (if session-name-provided-p
      (format-shell-command "xterm -class tmux -e tmux new-session -A -s ~A"
	session-name)
      (run-raise-or-pull "xterm -class tmux -e tmux new-session -A -s Main"
			 '((:class "tmux")))))

(defcommand lem () ()
  (run-raise-or-pull "xterm -class lem -e tmux new-session -A -s Lem" '(:class "Lem")))

(defcommand appfinder () () 
  (run-raise-or-pull "xfce4-appfinder" '((:class "Xfce4-appfinder"))))

(defcommand xfce4-terminal () ()
  "run xfce terminal"
  (run-raise-or-pull "xfce4-terminal" '(:class "Terminal")))

(defcommand gparted () ()
  (run-shell-command "gparted"))

(defcommand darktable () () 
  (run-raise-or-pull "darktable" '(:class "Darktable")))

(defcommand gimp () () 
  (run-raise-or-pull "gimp" '(:class "GIMP")))

(defcommand pulse-audio () ()
  (run-raise-or-pull "pavucontrol" '(:class "Pavucontrol")))

(defcommand calibre () ()
  (run-raise-or-pull "calibre" '(:class "calibre")))

(defcommand newsboat () ()
  (run-raise-or-pull  "xterm -class Newsboat -e newsboat" '(:class "newsboat")))

(defcommand xlock () ()
  (run-shell-command "xlock"))

;;; password vault and otp/2fa 

(defcommand bitwarden () ()
  (run-raise-or-pull "bitwarden-bin" '(:class "Bitwarden")))

(defcommand otp () ()
  (run-raise-or-pull "otpclient" '(:class "OTPClient")))

(defcommand 2fa-client () ()
  (run-raise-or-pull "otpclient" '(:class "OTPClient")))

;;; manage vpn via stump menus. 

(defcommand switch-vpn () ()
  "this command hangs stumpwm for the duration of protonvpn-cli operation. this 
is intended to prevent the user from doing anything while the vpn is setting up."
  (let ((connection
	 (second
	  (select-from-menu (current-screen)
			    `(("US netflix" "-c US-WA#1 udp")
			      ("Iceland" "-cc IS")
			      ;; ("choose" "-c")
			      )
			    "Select vpn server to connect to: "))))
    (run-sudo-shell-command (format nil "protonvpn-cli -d && sudo protonvpn-cli ~A" connection) t)))
