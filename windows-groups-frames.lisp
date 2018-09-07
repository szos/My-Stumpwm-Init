(in-package :stumpwm)

;; Window styling
(setf *window-format* "%n%s%c")
(set-unfocus-color (first *colors*))
(set-focus-color (cadr *colors*))
(set-win-bg-color ;; (caddr (cddddr *colors*))
		  (car *colors*))
(setf *normal-border-width* 3)
(setf *window-border-style* :thick)

(when *initializing* 
  (grename "Home")
  (gnewbg "Media")
  (gnewbg "Dev")
  (run-shell-command "nm-applet")
  (run-shell-command "./pia.sh"))

(defcommand pulley (search-term) ((:string "search term:  "))
  (when-let ((win (fuzzy-finder `((:class ,search-term)))))
    (pull-window win)))
	    

;; some functions for dealing with multiple groups and raise/pull requests
(defun pull (win &optional (all-groups *run-or-raise-all-groups*))
  "currently only supports pulling from all groups. todo: implement all screens."
  (if (not (equal (window-group win) (current-group)))
      (if all-groups
	 (progn
	   (move-window-to-group win (current-group))
	   (pull-window win))
	 (message "Window is not in the current group, and all-groups is nil."))
      (pull-window win)))

(defun raise (win &optional (all-groups *run-or-raise-all-groups*))
  "raise the window. "
  (if (equal (window-group win) (current-group))
      (focus-all win)
      (if all-groups
	  (focus-all win)
	  (message "Window not in current group, all groups is nil"))))

(defun frame-closest-to-point (point &optional (group (current-group)))
  "takes a point and returns the frame closest to it. 
point is in form (x . y)"
  (let ((frames (group-frames group))
	(frame-num nil)
	(possible-frames-xy-alist nil)
	(track nil))
    (setf possible-frames-xy-alist
	  (mapcar #'(lambda (frame)
		      `(,(sqrt (+ (expt (- (frame-x frame) (car point)) 2)
				  (expt (- (frame-y frame) (cdr point)) 2)))
			 . ,(frame-number frame)))
		  frames))
    (mapcar #'(lambda (x)
		(let ((dist (car x)))
    		  (when (or (not track) (< dist track))
    		    (setf track dist))))
    	    possible-frames-xy-alist)
    (setf frame-num (cdr (assoc track possible-frames-xy-alist)))
    (frame-by-number group frame-num)))

(defun unfloat-window (window group)
  ;; maybe find the frame geometrically closest to this float?
  (let ((frame (frame-closest-to-point (cons (window-x window)
					     (window-y window)))))
    (change-class window 'tile-window :frame frame)
    (setf (window-frame window) frame
          (frame-window frame) window
          (tile-group-current-frame group) frame)
    (update-decoration window)
    (sync-frame-windows group frame)))

(defun float-window (window group)
  (let ((frame (tile-group-current-frame group)))
    (change-class window 'float-window)
    (float-window-align window)
    (funcall-on-node (tile-group-frame-tree group)
                     (lambda (f) (setf (slot-value f 'window) nil))
                     (lambda (f) (eq frame f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fuzzy finding windows ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-list (l)
  (if l
      (if (atom l)
	  (list l)
	  (mapcan #'flatten-list l))))

(defun window-matches-properties-fuzzy (window &key class instance type role title)
  "Returns T if window matches all the given properties"
  (and
   (if class (search class (window-class window)) t)
   (if instance (search instance (window-res window)) t)
   (if type (search type (window-type window)) t)
   (if role (search role (window-role window)) t)
   ;; (if role 
   ;;     (string-match (window-role window) role) t)
   (if title (search title (window-title window)) t)
   t))

(defun find-matching-windows-fuzzy (props all-groups all-screens)
  "Returns list of windows containing @var{props}. eg if its passed 'h' 
all windows containing h in the property are listed @var{all-groups} 
will find windows on all groups. Same for @{all-screens}. Result is sorted 
by group and window number, with group being more significant (think radix sort)."
  (let* ((screens (if all-screens
                      *screen-list*
                      (list (current-screen))))
         (winlist (if all-groups
                      (mapcan (lambda (s) (screen-windows s)) screens)
                      (group-windows (current-group))))
         (matches (remove-if-not (lambda (w)
                                   (apply 'window-matches-properties-fuzzy w props))
                                 winlist)))
    (stable-sort (sort matches #'< :key #'window-number)
                 #'< :key (lambda (w) (group-number (window-group w))))))


(defun fuzzy-finder (&optional 
		       (props '(:class "") props-supplied-p)
		       (fmt *window-format*)
		       (all-groups *run-or-raise-all-groups*)
		       (all-screens *run-or-raise-all-screens*))
  "returns a window chosen by the user after selection from a windowlist
derived from the properties sent in. if no properties are sent in it defaults
to collecting all windows. if no windows are found returns nil, if one found 
returns the window, otherwise user selects window from menu. "
  (if props-supplied-p
    (let ((matches (flatten-list
                    (loop for x in props
                          collect (find-matching-windows-fuzzy x all-groups all-screens)))))
      (case (length matches)
        (0 (message "Nothing Found..."))
      	(1 (car matches))
        (t (select-window-from-menu matches fmt))))
    (let ((matches (find-matching-windows-fuzzy props all-groups all-screens)))
      (case (length matches)
	(0 (message "Nothing Found..."))
	(1 (car matches))
	(t (select-window-from-menu matches fmt))))))

(defcommand test-fuzzy-function (str type) ((:string "Search for: ")
					    (:string "type: "))
  (when-let ((win (fuzzy-finder `((,(if (char= (char type 0) #\:)
					(read-from-string type)
					(read-from-string (concatenate 'string ":" type)))
				    ,str)))))
    (eval (cadr (select-from-menu (current-screen)
				  `(("pull" (pull-window ,win))
				    ("raise" (raise-window ,win))
				    ("focus" (focus-all ,win))
				    ("delete" (delete-window ,win))))))))

(defcommand find-then-do (str type) ((:string "Search term: ")
				     (:string "Property to search: "))
  "find a window, then choose to pull, raise or focus it. "
  (let ((win (fuzzy-finder `((,(if (char= (char type 0) #\:)
				   (read-from-string type)
				   (read-from-string (concatenate 'string ":" type)))
			       ,str)))))
    (eval (cadr (select-from-menu (current-screen)
				  `(("pull" (pull ,win))
				    ("raise" (raise ,win))
				    ("focus" (focus-all ,win))
				    ("delete" (delete-window ,win))
				    ("execute" (run-shell-command))
				    ("run-command" (colon))))))))

(defcommand do-then-find (str type) ((:string "Search term: ")
				     (:string "Property to search: "))
  "choose what to do (raise or pull) then find the window."
  (let ((arg `((,(if (char= (char type 0) #\:) ;; check if the user provided a : or not. if not
		     (read-from-string type) ;; push one on to generate the correct symbols for the
		     (read-from-string (concatenate 'string ":" type))) ;; fuzzy finder
		 ,str))))
    (eval (cadr 
	   (select-from-menu 
	    (current-screen) 
	    `(("pull" (pull (fuzzy-finder ',arg)))
	      ("raise" (raise (fuzzy-finder ',arg)))
	      ("focus" (focus-all (fuzzy-finder ',arg)))
	      ("delete" (delete-window (fuzzy-finder ',arg)))
	      ("execute" (run-shell-command))
	      ("run command with window" (colon ;; "run command with window: "
					  ))))))))

(defcommand test-usage-pull/run-cmd (cmd pulley) ((:shell "exec: ")
						  (:rest "or pull: "))
  "This command takes a shell command and a substring to search for in the title 
of windows. If no windows match or the user quits at the window menu the shell 
command is run. Otherwise the selected window is pulled. "
  (if-let ((win (fuzzy-finder `((:title ,pulley)))))
    (pull-window win)
    (run-shell-command cmd)))





