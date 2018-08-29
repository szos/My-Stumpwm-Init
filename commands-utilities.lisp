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
(defun keyboard-layout-setter ()
  (let ((layout-list '("us" "no")))
    (lambda (&optional (layout "us" layout-supplied-p))
      (cond (layout-supplied-p
	     (run-shell-command (format nil "setxkbmap ~S" layout)))
	    (t
	     (setf layout-list 
		   (let ((a (pop layout-list)))
		     (append layout-list (list a))))
	     (run-shell-command (format nil "setxkbmap ~S" (car layout-list)))
	     (message "Current layout: ~S" (car layout-list)))))))

(defparameter *layout* (keyboard-layout-setter))
(defcommand cycle-layout () ()
  (funcall *layout*))
;; set up bightness control
;; set up a closure based implementation:

(defun brightness ()
  (run-shell-command "xbacklight -inc 100")
  (let ((level 100))
    (lambda (inter)
      (cond ((= inter 1) ;;increase brightness
	     (progn
	       (unless (>= level 100) 
		 (run-shell-command "xbacklight -inc 10")
		 (setf level (+ level 10)))
	       (message "Brightness: ~D%" level)
	       (sleep .2)))
	    ((= inter -1) ;;decrease brightness
	     (unless (<= level 0)
	       (run-shell-command "xbacklight -dec 10")
	       (setf level (- level 10)))
	     (message "Brightness: ~D%" level)
	     (sleep .2))
	    ((= inter 0) ;; go to 1% brightness
	     (setf level 1)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2)
	     (run-shell-command "xbacklight -inc 1")
	       )
	    ((= inter 2) ;; reset to 10% brightness
	     (setf level 10)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2)
	     (run-shell-command "xbacklight -inc 10")
	     (message "Brightness: ~D%" level))
	    (t
	     (message "Brightness: ~D%" level))))))

(defparameter *brightness* (brightness))

(defcommand brightness-change (inc) ((:number "1, 0, -1, or 2: "))
  (funcall *brightness* inc))

(defcommand brightness-reset () ()
  (funcall *brightness* 2))

;; volume tracking variable and initialization
;; make another closure called overdrive, which when we increase nmax
;; in the volume closure, we instead get thrown to overdrive which
;; displays how far over 100% we are instead of our normal volume bar.

(defun volume-setter ()
  (run-shell-command "pactl set-sink-volume 0 0%")
  (let ((max 100)
	(tracker 0)
	(vol-str-const "[*----------]"))
    (lambda (change &optional (nmax 100 nmax-supplied-p))
      (when nmax-supplied-p
	(setf max nmax))
      (cond ((> (+ tracker change) max)
	     (message "Max Volume.  ~D%" tracker))
	    ((< (+ tracker change) 0)
	     (run-shell-command "pactl set-sink-volume 0 0%")
	     (message "Min Volume. ~D%" tracker))
	    ((= max 100)
;;; this is disgusting. you need to refactor this..
	     (setf tracker (+ tracker change))
	     (setf (subseq *volume-std* 5 (+ (/ tracker 10) 5)) "=============")
	     (setf (subseq *volume-std* (+ (/ tracker 10) 5) 16) "§------------")
	     (setf *volume-readout* (concatenate 'string *volume-std* *volume-endcap*))
	     (message "Volume: ~D%" tracker)
	     (run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change)))
	    (t
	     (if (> (+ tracker change) 100)
		 (progn
		   (setf tracker (+ tracker change))
		   (setf (subseq *volume-std* 5 16) "============")
		   (setf (subseq *volume-overdrive* 2 (IF (> (+ (/ TRACKER 10) 2) 11)
							  11
							  (+ (/ TRACKER 10) 2)))
			 "XXXXXXXXXXXX"))))))))
		   
	     ;; (cond ((> max 100)
	     ;; 	    (setf (subseq *volume-overdrive* 1 (+ (/ (- tracker 100) 10) 1)) "XXXXXXXXXX")
	     ;; 	    ;; (setf (subseq *volume-overdrive* (+ (/ (- tracker 100) 10) 10) 12) "~~~~~~~~~~")
	     ;; 	    (setf *volume-readout* (concatenate 'string *volume-std*
	     ;; 						*volume-overdrive* *volume-endcap*))
	     ;; 	    (message "WARNING: OVERDRIVE || VOL: ~D%" tracker))
	     ;; 	   ((= max 100)
	     ;; 	    (setf (subseq *volume-std* 5 (+ (/ tracker 10) 5)) "=============")
	     ;; 	    (setf (subseq *volume-std* (+ (/ tracker 10) 5) 16) "§------------")
	     ;; 	    (setf *volume-readout* (concatenate 'string *volume-std* *volume-endcap*))
	     ;; 	    (message "Volume: ~D%" tracker)
	     ;; 	    (run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change))))

(defparameter *volume-readout* "^7^B[$----------]")
(defparameter *volume-std* "^7^B[$----------]") 
(defparameter *volume-endcap* "^6^b")
(defparameter *volume-overdrive* "[~~~~~~~~~~]")

;; (setf (subseq *string-test* 1 5) "==========")

;; volume readout: 
;; [==========*]
;; 
;; [==========*]^7^B[===*-------]^6^b
;;
;; [===*-------]
;; [=====*-----]
;; [+++<¤¤¤¤¤¤¤]
;; []
;; [######-----]
;; [$$$$$$$*###]
;; [####*~~~~~~]
;; [§§§§§>¤¤¤¤¤]

(defparameter *volume* (volume-setter))

(defcommand volume (inc) ((:number "enter the increment:  "))
  (funcall *volume* inc))
(defcommand vol-overdrive (new) ((:number "new max vol"))
  (funcall *volume* 0 new))
(defcommand vol-reset () ()
  ;(defparameter *volume-readout* "^7^B[$----------]")
  ;(defparameter *volume-std* "^7^B[$----------]") 
  ;(defparameter *volume-endcap* "^6^b")
  (defparameter *volume* (volume-setter))
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
  ((kbd "C-l") "cycle-layout")
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
C-l--------------Cycle Keyboard Layouts
C-m-------Toggle Modeline and Stumptray
M-t------------Cycle Screen Temperature
C-h----------------------This Help Menu"))


