(in-package :stumpwm)

(defmacro cond-let (vars &body cond-body)
  `(let ,vars
     (cond ,@cond-body)))

(defun current-frame ()
  (window-frame (current-window)))

(defcommand sib () ()
  (actually-switch-to-sibling))

(defun actually-switch-to-sibling ()
  (let ((sib (switch-to-sibling)))
    (if (listp sib)
	(curframe)
	(focus-frame (current-group) sib))))

;; (((#S(frame 0 #S(TILE-WINDOW "Futurama S02E11 The Lesser Of Two Evils - VLC media player" #x3000006) 0 0 960 540)
;;    #S(frame 2 #S(TILE-WINDOW "Debugging Lisp Part 2: Inspecting – malisper.me - Mozilla Firefox" #x1A00A2C) 0 540 960 540))
;;   #S(frame 1 #S(TILE-WINDOW "emacs@Gypsy" #x36000AB) 960 0 960 1080)))
;; is structured like so:
;; (((frame frame)
;;   frame))
;; while
;; (#S(frame 0 #S(TILE-WINDOW "emacs@Gypsy" #x36000AB) 0 0 1920 1080))
;; is structured:
;; (frame)
;; while
;; ((#S(frame 0 #S(TILE-WINDOW "emacs@Gypsy" #x36000AB) 0 0 960 1080)
;;   #S(frame 1 #S(TILE-WINDOW "Futurama S02E11 The Lesser Of Two Evils - VLC media player" #x3000006) 960 0 960 1080)))
;; is like:
;; ((frame frame))

(defun switch-to-sibling (&optional (frame-tree (tile-group-frame-tree (current-group)))
			    (frame-to-match (current-frame)))
  "takes a frame-tree and a frame to find the sibling of. it returns the sibling, or a 
list of the sibling frames, if the sibling is divided by splits."
  (when frame-tree
    (let ((left (first frame-tree))
	  (right (second frame-tree))
	  (result nil))
      ;; (break)
      ;; were taking in a list, which has two items. think bst. (frame-or-list frame-or-list)
      ;; so we have to examine left and right sides to find our frame.
      (when (frame-p left)
	(print "checking left")
	(when (eq frame-to-match left)
	  (print "it was left")
	  (return-from switch-to-sibling right))) ;; return a list containing the match, then the sibling.
      (when (frame-p right)
	(print "checking right")
	(when (eq frame-to-match right)
	  (print "it was right, returning up a level")
	  (return-from switch-to-sibling left))) ;; return the sibling. 
      (when (listp left)
	(print "entering next level, on the left")
	(setf result (switch-to-sibling left frame-to-match))
	
	(let ((ret (switch-to-sibling left frame-to-match)))
	  (if (eq frame-to-match ret)
	      (return-from switch-to-sibling ret)
	      :noip))
	;;(switch-to-sibling left frame-to-match)
	)
      (when (listp right)
	(print "entering next level, on the right")
	(return-from switch-to-sibling (switch-to-sibling right frame-to-match))
	;;(switch-to-sibling right frame-to-match)
	))))

(defun to-sibling (&optional (frame-tree (tile-group-frame-tree (current-group)))
		     (frame-to-match (current-frame)))
  "takes a frame-tree and a frame to find the sibling of. it returns the sibling, or a 
list of the sibling frames, if the sibling is divided by splits."
  (when frame-tree
    (let ((left (first frame-tree))
	  (right (second frame-tree)))
      ;; (break)
      ;; were taking in a list, which has two items. think bst. (frame-or-list frame-or-list)
      ;; so we have to examine left and right sides to find our frame.
      (cond ((frame-p left)
	     (when (eq frame-to-match left)
	       (if right
		   right
		   :right-is-nil)))
	    ((frame-p right)
	     (when (eq frame-to-match right)
	       (if left
		   left
		   :left-is-nil)))
	    ((listp left)
	     (to-sibling left frame-to-match))
	    ((listp right)
	     (to-sibling right frame-to-match))
	    (t
	     frame-to-match)))))

(defun map-on-frame-tree (frame-tree frame-to-match)
  "frame-tree is a list of frames. its organized with two items per level in the list,
and those items can be a frame, or another level(list) holding two more items. everything
descends from one frame, which gets split.  eg:
one frame is:      (#S(frame))
split vertical is: ((#S(frame) #S(frame))) - this is the top frame, split into two frames. 
this is represented by the list of frames. 


;((#S(frame) #S(frame))) top frame divided in two. 
;((#S(frame) (#S(frame) #S(frame)))) top frame divided in two, one of those frames divided in two.
"
  (when frame-tree
    (let ((left (first frame-tree))
	  (right (second frame-tree))
	  (examine-left nil)
	  (examine-right nil))
      ;; were taking in a list, which has two items. think bst. (frame-or-list frame-or-list)
      ;; so we have to examine left and right sides to find our frame.
      (when (frame-p left)
	(when (eq frame-to-match left)
	  (return-from map-on-frame-tree left)))
      (when (frame-p right)
	(when (eq frame-to-match right)
	  (return-from map-on-frame-tree right)))
      (when (listp left)
	(setf examine-left (map-on-frame-tree left frame-to-match))
	(if (eq frame-to-match examine-left)
	    (return-from map-on-frame-tree examine-left)))
      (when (listp right)
	(setf examine-right (map-on-frame-tree right frame-to-match))
	(if (eq frame-to-match examine-right)
	    (return-from map-on-frame-tree examine-right)))
      ;; (cond ((frame-p left)
      ;; 	     (when (eq frame-to-match left)
      ;; 	       right))
      ;; 	    ((frame-p right)
      ;; 	     (when (eq frame-to-match right)
      ;; 	       left))
      ;; 	    ((listp left)
      ;; 	     (let ((ret (map-on-frame-tree left frame-to-match)))
      ;; 	       (if (eq frame-to-match ret)
      ;; 		   ret)))
      ;; 	    ((listp right)
      ;; 	     (let ((ret (map-on-frame-tree right frame-to-match)))
      ;; 	       (if (eq frame-to-match ret)
      ;; 		   ret)))
      ;; 	    (t
      ;; 	     nil))
      )))

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

