(in-package :stumpwm)

(add-screen-mode-line-formatter #\z 'test/fmt-head-window-list)
(defparameter *fmt-windows-length* 125)
(defun test/fmt-head-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (let* ((wins (sort1 (head-windows (mode-line-current-group ml)
				    (mode-line-head ml))
		      #'< :key #'window-number))
	 (winlen (floor (/ *fmt-windows-length*
			   (if (= 0 (length wins)) 1 (length wins)))))
	 (reter (format nil "~{~a~^ ~}"
			 (mapcar (lambda (w)
				   (let ((str (format-expand *window-formatters*
							     *window-format* w)))
				     (when (> (length str) winlen)
				       (setf str (subseq str 0 winlen)))
				     (if (eq w (current-window))
					 (fmt-highlight str)
					 str)))
				 wins))))
    (let ()
      (if (< *fmt-windows-length* (length reter))
	  (format nil "~{~a~^ ~}"
		  (mapcar (lambda (w)
			    (let ((str (format-expand *window-formatters*
						      *window-format* w)))
			      (when (> (length str) winlen)
				(setf str (subseq str 0 winlen)))
			      (cond ((eq w (current-window))
				     (fmt-highlight (subseq str 0 4)))
				    ((window-visible-p w)
				     (subseq str 0 4))
				    (t str))))
			  wins))
	  reter))))

(setf *colors* (list "black"   "red"  "green" "yellow"  "blue"
		     "magenta" "cyan" "white" "#777777" "orange"))

(update-color-map (current-screen))

(defun battery-check (ml)
  (declare (ignore ml))
  (let ((percentage
	 (parse-integer (unix-cat "/sys/class/power_supply/BAT0/capacity")))
	(status (unix-cat "/sys/class/power_supply/BAT0/status")))
    (format nil "^[~a~a^]%^[~a^]"
	    (cond ((> 15 percentage) "^(:fg \"red\")")
		  ((> 40 percentage) "^(:fg \"orange\")")
		  ((> 70 percentage) "^(:fg \"yellow\")")
		  (t "^(:fg \"green\")"))
	    percentage
	    (cond ((string= status "Charging")
		   "^(:fg \"green\")+")
		  ((string= status "Discharging")
		   "^(:fg \"red\")-")
		  (t "")))))

(add-screen-mode-line-formatter #\B 'battery-check)

(defun memory-check (ml)
  (declare (ignore ml))
  (let ((p (get-memory-usage-percent)))
    (format nil "^[~a~a^]"
	    (cond ((> 25 p) "^(:fg \"green\")")
		  ((> 50 p) "^(:fg \"yellow\")")
		  ((> 70 p) "^(:fg \"orange\")")
		  (t "^(:fg \"red\")"))
	    (ceiling p))))

(add-screen-mode-line-formatter #\M 'memory-check)

(defun my/bar (percent width full empty)
  "Return a progress bar string of WIDTH characters composed of characters FULL
  and EMPTY at PERCENT complete."
  (let ((chars (truncate (* (/ width 100) percent)))
	(*bar-med-color* "^B^22*")
	(*bar-hi-color* "^B^33*")
	(*bar-crit-color* "^B^11*"))
    (format nil "^[~A~A^]~A" (bar-zone-color percent)
            (repeat chars full)
            (repeat (- width chars) empty))))

(defun my-cpu-usage (ml)
  (declare (ignorable ml))
  (if (member "cpu" (list-loaded-modules) :test #'string=)
      (with-open-files
	  ((t1 "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp1_input")
	   (t2 "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp2_input")
	   (t3 "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp3_input"))
	(let* ((temperature (/ (/ (+ (read t1) (read t2) (read t3)) 3) 1000))
	       (cpu (truncate (* 100 (cpu::current-cpu-usage))))
	       (color (cond ((>= cpu 75)
			     "^(:fg \"red\")")
			    ((>= cpu 50)
			     "^(:fg \"orange\")")
			    ((>= cpu 25)
			     "^(:fg \"yellow\")")
			    (t
			     "^(:fg \"green\")"))))
	  (format nil "^[~A~A^]% ~A°" color cpu (ceiling temperature))))
      "Unknown"))

(add-screen-mode-line-formatter #\U 'my-cpu-usage)

(setf *screen-mode-line-format*
      (list "^[^68[^58%g^68]%z^>^6^BCPU:%U MEM:%M% BAT:%B %h^b%T"))

(setf *mode-line-timeout* 5)
