(in-package :stumpwm)

;; test keypress display message:
(defun display-key-press-hook (key key-seq cmd)
  "a function that displays the key being pressed in the lower right corner. "
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :bottom-right))
      (message "Key Sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd)
      (sleep 0.5))))

(defcommand toggle-key-readout () ()
  (if (member 'display-key-press-hook *key-press-hook*)
      (remove-hook  *key-press-hook* 'display-key-press-hook)
      (add-hook *key-press-hook* 'display-key-press-hook)))

;; sections: 
;; Keyboard layout
;; layout tracking variable
;; (defun keyboard-layout-setter ()
;;   (let ((layout-list '("us" "no")))
;;     (lambda (&optional (layout "us" layout-supplied-p))
;;       (cond (layout-supplied-p
;; 	     (run-shell-command (format nil "setxkbmap ~S" layout)))
;; 	    (t
;; 	     (setf layout-list 
;; 		   (let ((a (pop layout-list)))
;; 		     (append layout-list (list a))))
;; 	     (run-shell-command (format nil "setxkbmap ~S" (car layout-list)))
;; 	     (run-shell-command "xmodmap ~/.stumpwm.d/kcode.modmap")
;; 	     ;; (when (string= (car layout-list) "no")
;; 	     ;;   (set-prefix-key (kbd "C-")))
;; 	     (message "Current layout: ~S" (car layout-list)))))))

;; (meta (kbd "ISO_Level3_Latch-z"))

;;; (defparameter *layout* (keyboard-layout-setter))
;; (defcommand cycle-layout () ()
;;   (funcall *layout*))
;; set up bightness control
;; set up a closure based implementation:
(defun bright-readout-set (amnt)
  "helper for brightness. this just resets the brightness readout
for the mode line. "
  (cond ((>= amnt 100)
	 (setf *brightness-mode-line* (format nil "^B100^b")))
	((<= amnt 0)
	 (setf *brightness-mode-line* (format nil "^B  0^b")))
	(t
	 (cond ((< amnt 10)
		(setf *brightness-mode-line* (format nil "^B  ~D%^b" amnt)))
	       ((and (>= amnt 10) (< amnt 100))
		(setf *brightness-mode-line* (format nil "^B ~D%^b" amnt)))
	       (t
		(setf *brightness-mode-line* (format nil "^B~D%^b" amnt))))))
  (message "Brightness: ~D%" amnt))

(defun abso (x)
  (if (< x 0)
      (+ x x x)
      x))

(defun brightness-setter ()
  (run-shell-command "xbacklight -inc 100")
  (let ((level 100))
    (lambda (change)
      (cond ((< change 0)
	     ;; decrement and check if 0
	     ())
	    ((> change 0)
	     ;; increment and check if 100
	     )
	    ((>= (+ level change) 100)
	     (setf *brightness-mode-line* (format nil "^B100^b"))
	     (run-shell-command "xbacklight -inc 100"))
	    ((< (+ level change) 10)
	     (run-shell-command (format nil "xbacklight -dec ~D" (abs change)))
	     (setf *brightness-mode-line* (format nil "^B  0^b")))
	    (t
	     ())
	    ))))
(defun brightness ()
  (run-shell-command "xbacklight -inc 100")
  (let ((level 100))
    (λ (inter)
      (cond ((= inter 1) ;;increase brightness
	     (progn
	       (unless (>= level 100) 
		 (run-shell-command "xbacklight -inc 5")
		 (setf level (+ level 5))
		 (setf *brightness-mode-line* (format nil "")))))
	    ((= inter -1) ;;decrease brightness
	     (unless (<= level 0)
	       (run-shell-command "xbacklight -dec 5")
	       (setf level (- level 5))))
	    ((= inter 0) ;; go to 1% brightness
	     (setf level 1)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2))
	    ((= inter 2) ;; reset to 10% brightness
	     (setf level 10)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2)
	     (run-shell-command "xbacklight -inc 10"))
	    (t
	     nil))
      (setf *brightness-mode-line* (format nil "Brightness: ~a%"
					   (cond ((< level 10)
						  (format nil "  ~D" level))
						 ((= level 100)
						  "100")
						 (t
						  (format nil " ~D" level)))))
      nil)))

(defparameter *brightness* (brightness))
(defparameter *brightness-mode-line* "100")

(defcommand brightness-change (inc) ((:number "1, 0, -1, or 2: "))
  (let ((*timeout-wait* 1))
    (funcall *brightness* inc)))

(defcommand brightness-reset () ()
  (funcall *brightness* 2))

;; volume tracking variable and initialization
;; make another closure called overdrive, which when we increase nmax
;; in the volume closure, we instead get thrown to overdrive which
;; displays how far over 100% we are instead of our normal volume bar.
(defparameter *volume-percentage* "Volume:   0% ")

;;;; IF YOUR HAVEING TROUBLE WITH VOLUME CONTROLS NOT WORKING,
;;;; RUN ALSAMIXER, THEN THEY SHOULD WORK.

(defun volume-setter ()
  "this function generates a closure for controlling volume via the shell 
command \"pactl set-sink-volume 0\". this closure also interacts with the 
dynamic variable *volume-percentage*, which is designed to be evaluated in 
the mode line to show the volume "
  (run-shell-command "pactl set-sink-volume 0 0%")
  (let ((max 100)
	(tracker 0))
    (lambda (change)
      (cond ((> (+ tracker change) max)
	     (setf *volume-percentage* "Volume: ^1^B100%^^b^6 ")
	     (message "Max Volume.  ~D%" tracker))
	    ((< (+ tracker change) 0)
	     (run-shell-command "pactl set-sink-volume 0 0% ")
	     (setf *volume-percentage* "Volume: ^2^B0%^^b^6 ")
	     (message "Min Volume. ~D%" tracker))
	    (t
	     (setf tracker (+ tracker change))
	     (cond ((< tracker 10)
		    ;; format green and add spaces
		    (setf *volume-percentage* (format nil "Volume: ^2^B  ~D%^^b^6 " tracker)))
		   ((= tracker 100)
		    (setf *volume-percentage* (format nil "Volume: ^1^B~D%^^b^6 " tracker)))
		   ((<= tracker 50)
		    ;;format green
		    (setf *volume-percentage* (format nil "Volume: ^2^B ~D%^^b^6 " tracker)))
		   ((and (> tracker 50) (<= tracker 75))
		    ;;format yellow
		    (setf *volume-percentage* (format nil "Volume: ^3^B ~D%^^b^6 " tracker)))
		   (t
		    ;;format red 
		    (setf *volume-percentage* (format nil "Volume: ^1^B ~D%^^b^6 " tracker))))
	     (run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change)))))))

(defun volume-setter-nmax ()
  "this function generates a closure for controlling volume via the shell 
command \"pactl set-sink-volume 0\". this closure also interacts with the 
dynamic variable *volume-percentage*, which is designed to be evaluated in 
the mode line to show the volume "
  (run-shell-command "pactl set-sink-volume 0 0%")
  (let ((max 100) ;; what to consider the maximum
	(limit 200) ;; the highest value max can be
	(tracker 0))
    (lambda (change &optional (nmax 100 nmax-provided-p))
      (when (and nmax-provided-p (<= nmax limit))
	(setf max nmax))
      (cond ((> (+ tracker change) max)
	     (setf *volume-percentage* "Volume: ^1^B100%^^b^6 ")
	     (setf tracker max)
	     (message "Max Volume.  ~D%" tracker)
	     (sleep .2))
	    ((< (+ tracker change) 0)
	     (run-shell-command "pactl set-sink-volume 0 0% ")
	     (setf *volume-percentage* "Volume: ^2^B0%^^b^6 ")
	     (message "Min Volume. ~D%" tracker)
	     (sleep .2))
	    (t
	     (setf tracker (+ tracker change))
	     (cond ((< (* (/ tracker max) 100) 10)
		    ;; format green and add spaces
		    (setf *volume-percentage* (format nil "Volume: ^2^B  ~D%^^b^6 " (round (* (/ tracker max) 100)))))
		   ((= (* (/ tracker max) 100) 100)
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
	     (run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change))
	     )))))

(defparameter *volume* (volume-setter-nmax))

(defcommand volume-overdrive (nmax) ((:number "New maximum volume (default is 100):  "))
  (funcall *volume* 0 nmax))
(defcommand reset-overdrive () ()
  (funcall *volume* 0 100))

(defcommand volume (inc) ((:number "enter the increment:  "))
  (funcall *volume* inc))
(defcommand vol-reset () ()
  ;(defparameter *volume-readout* "^7^B[$----------]")
  ;(defparameter *volume-std* "^7^B[$----------]") 
  ;(defparameter *volume-endcap* "^6^b")
  (defparameter *volume* (volume-setter-nmax))
  (volume 0))


;; tracks the status of the mode line to ensure stumptray is safely enabled and disabled. 
;; mode line is off --- 0
;; mode line is on ---- 1
(defun mode-liner ()
  (let ((mode-line-tracker 0))
    (lambda ()
      (cond ((= mode-line-tracker 0)
	     (mode-line)
	     (stumptray:stumptray)
	     (setf mode-line-tracker 1))
	    ((= mode-line-tracker 1)
	     (stumptray:stumptray)
	     (mode-line)
	     (setf mode-line-tracker 0))
	    (t
	     (message "there is something wrong with the closure from the function mode-liner"))))))

(defparameter *mode-line* (mode-liner))
(when *initializing* (funcall *mode-line*))

(defcommand modeline-stumptray-toggle () ()
  (funcall *mode-line*))

(defun scrn-temper ()
  (let ((tracker "Neutral")
	(night 4500)
	(late-night 3000))
    (lambda ()
      (run-shell-command "redshift -x")
      (cond ((equalp tracker "Neutral")
	     (run-shell-command (format nil "redshift -O ~D" night))
	     (setf tracker "Night"))
	    ((equalp tracker "Night")
	     (run-shell-command (format nil "redshift -O ~D" late-night))
	     (setf tracker "Late-Night"))
	    ((equalp tracker "Late-Night")
	     (setf tracker "Neutral"))
	    (t
	     (message "something needs debugging in commands-utilities-lisp..."))))))

(defparameter *temperature* (scrn-temper))

(defcommand cycle-temperature () ()
  (funcall *temperature*))


(define-interactive-keymap system-manipulation ()
  ((kbd "C-v") "volume -10")
  ((kbd "M-v") "volume 10")
  ((kbd "C-M-v") "vol-reset")
  ((kbd "C-b") "brightness-change -1")
  ((kbd "M-b") "brightness-change 1")
  ((kbd "C-M-b") "brightness-change 0")
  ((kbd "C-M-B") "brightness-reset")
  ;; ((kbd "C-l") "cycle-layout")
  ((kbd "C-m") "modeline-stumptray-toggle")
  ((kbd "M-t") "cycle-temperature")
  ((kbd "C-h") "sys-manip-help"))

(defcommand sys-manip-help () ()
  (message "::::System Manipulation Keybindings::::
C-v--------------Volume Decrement by 10
M-v--------------Volume Increment by 10
C-M-v----------------------Volume Reset
C-b----------------Brightness Decrement 
M-b----------------Brightness Increment
C-M-b-------------Engage Low-light Mode
C-M-B-----------Reset Brightness to 10%
C-l--not used----Cycle Keyboard Layouts
C-m-------Toggle Modeline and Stumptray
M-t------------Cycle Screen Temperature
C-h----------------------This Help Menu"))


