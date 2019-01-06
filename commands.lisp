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
    ((not win) ;; this means that fuzzy finder was quit by the user. 
     (let ((opt (second (select-from-menu (current-screen) `(("RUN" ,cmd)
     							     ("EXIT" nil))))))
       (if (stringp opt)
     	   (run-shell-command opt)
     	   (eval opt))))
    ((equalp win :not-found)
     (if (stringp cmd)
     	 (run-shell-command cmd)
     	 (eval cmd)))
    ((or (window-visible-p win) (not (eq (window-group win) (current-group))))
     (raise win))
    (t
     (pull win))))

;;; keep screen from sleeping

(defparameter *jiggle-timer*
  nil
  "a timer for moving the mouse to keep the screen awake")

(defcommand enable-jiggle () ()
  (when (timer-p *jiggle-timer*)
    (cancel-timer *jiggle-timer*))
  (setf *jiggle-timer*
	(run-with-timer 0 100 #'(lambda ()
				  (ratrelwarp 1 1)
				  (ratrelwarp -1 -1))))
  (message "Cursor jiggle enabled"))

(defcommand disable-jiggle () ()
  (when (timer-p *jiggle-timer*)
    (cancel-timer *jiggle-timer*)
    (message "Cursor jiggle disabled")))

;;; pull a window

(defcommand pullstr-all (str) ((:string "pull: "))
  ;; (let* ((*window-format* "%n%s%c => %30t")
  ;; 	 (win (fuzzy-finder `((:class ,str) (:title ,str) (:role ,str)))))
  ;;   (when (and win (not (equalp win :not-found)))
  ;;     (pull win)))
  (message "pulling ~A" str))

(defcommand pullstr (str) ((:string "pull: "))
  (let* ((*window-format* "%n%s%c => %30t")
	 (win (fuzzy-finder `((:class ,str)))))
    (when (and win (not (equalp win :not-found)))
      (pull win))))

(defcommand test (strng) (:string "pull: ")
  (message "echoing \"~A\"" strng))

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

(defcommand appfinder () () 
  (run-raise-or-pull "xfce4-appfinder" '((:class "Xfce4-appfinder"))))

(defcommand xfce4-terminal () ()
  "run xfce terminal"
  (run-raise-or-pull "xfce4-terminal" '(:class "Terminal")))

(defcommand gparted () ()
  (run-shell-command "gparted"))

(defcommand darktable () () 
  (run-raise-or-list "darktable" '(:class "Darktable")))

(defcommand gimp () () 
  (run-raise-or-list "gimp" '(:class "GIMP")))

(defcommand pulse-audio () ()
  (run-raise-or-pull "pavucontrol" '(:class "Pavucontrol")))

(defcommand calibre () ()
  (run-raise-or-pull "calibre" '(:class "calibre")))

(defcommand newsboat () ()
  (run-raise-or-pull
   '(with-open-window "xterm -e newsboat" "XTerm"
     #'reclassify-window "Newsboat")
   '(:class "Newsboat")))

(defcommand reconnect-vpn (type country) ((:string "Enter parameter to pass (-sc, -cc, etc):  ")
					  (:string "enter country to connect to:  "))
  )

