(in-package :stumpwm)

;;; pull and raise functions

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

;;; closures for system functionality

(defmacro format-shell-command (&body format-stuff)
  `(run-shell-command (format nil ,@format-stuff)))

(defparameter *volume-percentage* "")

(defun volume-setter-nmax ()
  "this function generates a closure for controlling volume via the shell 
command \"pactl set-sink-volume 0\". this closure also interacts with the 
dynamic variable *volume-percentage*, which is designed to be evaluated in 
the mode line to show the volume "
  (run-shell-command "pactl set-sink-volume 0 0%")
  (let ((max 100) ;; what to consider the maximum
	(limit 200) ;; the highest value max can be
	(tracker 0)
	)
    (lambda (change &optional (nmax 100 nmax-provided-p))
      (if (and nmax-provided-p (<= nmax limit)) ;; set a limit to the overdrive 
	  (setf max nmax)
	  (cond ((> (+ tracker change) max)
		 (setf *volume-percentage* "Volume: ^1^B100%^^b^6 ")
		 (setf tracker max)
		 (message "Max Volume.  ~D%" tracker)
		 (sleep .2))
		((< (+ tracker change) 0)
		 (run-shell-command "pactl set-sink-volume 0 0%")
		 (setf *volume-percentage* "Volume: ^2^B0%^^b^6 ")
		 (message "Min Volume. ~D%" tracker)
		 (sleep .2))
		(t
		 (setf tracker (+ tracker change))
		 (cond ((< (* (/ tracker max) 100) 10)
			;; format green and add spaces - less than 10%
			(setf *volume-percentage* (format nil "Volume: ^2^B  ~D%^^b^6 " (round (* (/ tracker max) 100)))))
		       ((= (* (/ tracker max) 100) 100) ;; max volume
			(setf *volume-percentage* (format nil "Volume: ^1^B~D%^^b^6 " (round (* (/ tracker max) 100)))))
		       ((<= (* (/ tracker max) 100) 50)
			;;format green
			(setf *volume-percentage* (format nil "Volume: ^2^B ~D%^^b^6 " (round (* (/ tracker max) 100)))))
		       ((and (> (* (/ tracker max) 100) 50) (<= (* (/ tracker max) 100) 75))
			;;format yellow
			(setf *volume-percentage* (format nil "Volume: ^3^B ~D%^^b^6 " (round (* (/ tracker max) 100)))))
		       (t
			;;format red 
			(setf *volume-percentage* (format nil "Volume: ^1^B ~D%^^b^6 " (round (* (/ tracker max) 100))))))
		 ;;(run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change))
		 (format-shell-command "pactl set-sink-volume 0 ~D%" tracker)
		 (message "Volume: ~D%" tracker)))))))

(defparameter *volume* (volume-setter-nmax))

(defcommand volume-overdrive (nmax) ((:number "New maximum volume (default is 100):  "))
  (let ((*timeout-wait* 1))
    (funcall *volume* 0 nmax)
    (funcall *volume* 0))
  ;;(funcall *volume* -5)
  )

(defcommand volume-set (inc) ((:number "enter volume:  "))
  (let ((*timeout-wait* 1))
    (setf *volume* (volume-setter-nmax))
    (funcall *volume* inc)))

(defcommand volume (inc) ((:number "enter the increment:  "))
  (let ((*timeout-wait* 1))
    (funcall *volume* inc)))

(defun scrn-temper ()
  (let ((temps '#1=(4500 3000 0  . #1#))
	(tt 0))
    (lambda ()
      (setf tt (pop temps))
      (if (= tt 0)
	  (format-shell-command "redshift -x")
	  (format-shell-command "redshift -O ~D" tt)))))

(defparameter *temperature* (scrn-temper))

(defcommand cycle-temperature () ()
  (funcall *temperature*))

(defcommand redshift (amnt) ((:number "Temp:  "))
  "depends upon redshift command line utility. "
  (run-shell-command "redshift -x")
  (unless (= amnt 0)
    (format-shell-command "redshift -O ~D" amnt)))

(defun bright ()
  (run-shell-command "xbacklight -inc 100")
  (let ((level 100))
    (lambda (x)
      (cond ((and (> x 0) (< level 100))
	     (setf level (+ level x))
	     (format-shell-command "xbacklight -inc ~A" x)
	     (message "Brightness: ~A" level))
	    ((and (< x 0) (> level 0))
	     (setf level (+ level x))
	     (format-shell-command "xbacklight -dec ~A" (abs x))
	     (message "Brightness: ~A" level))
	    ((= x 0)
	     (setf level 0)
	     (format-shell-command "xbacklight -dec 100")))
      level)))

(defparameter *brightness* (bright))

(defcommand brightness-set (val) ((:number "Enter Brightness Level:  "))
  (let ((*timeout-wait* 1))
    (funcall *brightness* 0)
    (funcall *brightness* val)))

(defcommand brightness-increment (val) ((:number "Enter Brightness Increment:  "))
  (let ((*timeout-wait* 1))
    (funcall *brightness* val)))

(define-interactive-keymap change-frames (:on-enter #'fnext
						    :exit-on ()
						    ) 
  ((kbd "O") "fnext")
  ((kbd "o") "fnext")
  ((kbd "SPC") "fnext"))

