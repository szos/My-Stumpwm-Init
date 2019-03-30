(in-package :stumpwm)

(defmacro concat (&rest strings)
  "just cats the strings sent in."
  `(concatenate 'string ,@strings))

(defmacro my-when-let* (bindings &body do)
  (let* ((b1 (gensym))
	 (g (loop for bind in bindings
	       collect `(,(first bind)
			  (or ,(second bind)
			      (return-from ,b1 (values nil ',(second bind))))))))
    `(block ,b1
       (let* ,g
	 ,@do))))

(defmacro my-if-let* (bindings &body (then &optional else))
  (let ((b1 (gensym))
	(b2 (gensym)))
    `(block ,b1
       (block ,b2
	 (let* ,(loop for bind in bindings
		   collect `(,(car bind) (or ,(second bind)
					     (return-from ,b2 nil))))
	   (return-from ,b1 ,then)))
       ,else)))

(defun run-sudo-shell-command (cmd &optional collect-output-p)
  "takes a command, and creates a one liner for running it as root;
the one liner:  echo <password> | sudo -S <command>
this function prompts for the password instead of taking it as an argument 
in order to avoid exposing the root password in the .lisp file where this 
function is called. "
  (let* ((cmd-as-sudo
	  (concat (format nil "echo ~A | " (read-one-line (current-screen)
							  "password: "
							  :password t))
		  (format nil "sudo -S ~A" cmd))))
    (if collect-output-p
	(run-shell-command cmd-as-sudo t)
	(run-shell-command cmd-as-sudo))))

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

;; (define-syntax my-or
;;     (syntax-rules ()
;; 		  ((my-or) nil)
;; 		  ((my-or arg1) arg1)
;; 		  ((my-or arg1 arg2) (let ((temp arg1))
;; 				       (if temp temp arg2)))
;; 		  ((my-or arg1 arg2 arg3 ***) (my-or (my-or arg1 arg2) arg3 ***))))

(defun replace-newlines-in (string &key (with #\SPACE))
  (loop for character across string
     unless (char= character #\NEWLINE) collect character into list
     when (char= character #\NEWLINE) collect with into list
     finally (return (coerce list 'string))))

(defun format-volume-readout (string)
  "this loops through string, expecting it to be output from from 
amixer, piped through egrep to get the left and right levels, formatted like
so: 
\"xx%
xx%\"
and generates a string fit for messaging to the user."
  (loop with left = '(#\L #\e #\f #\t #\: #\SPACE #\SPACE)
     with right = '(#\R #\i #\g #\h #\t #\: #\SPACE)
     with switch? = nil
     for char across string
     do (if (char= char #\NEWLINE)
	    (progn (setf switch? t)
		   (setf left (append left (list #\SPACE))))
	    (if switch?
		(setf right (append right (list char))) ;; right channel
		(setf left (append left (list char))) ;; left channel
		))
     finally (return (coerce (concatenate 'list '(#\V #\o #\l #\u #\m #\e #\: #\SPACE #\SPACE)
					  left '(#\NEWLINE) '(#\SPACE #\SPACE #\SPACE #\SPACE #\SPACE #\SPACE #\SPACE #\SPACE #\SPACE)
					  right)
			     'string))))

(defun message-volume (&optional )
  (message
   (format-volume-readout
    (run-shell-command
     "amixer get Master | egrep -o \"[0-9]+%\" | egrep \"[0-9]*\"" t))))

(defun get-volume ()
  (let* ((amixer-report
	  (run-shell-command
	   "amixer get Master | egrep -o \"[0-9]+%\" | egrep \"[0-9]*\"" t))
	 (location
	  (mismatch amixer-report
		    (replace-newlines-in amixer-report :with #\_))))
    (parse-integer (subseq amixer-report 0 (- location 1)))))

(defun set-volume (percentage)
  (let* ((cmd-string
	  (format nil
		  "amixer sset Master ~A% | egrep -o \"[0-9]+%\" | egrep \"[0-9]*\"" percentage))
	 (result (run-shell-command cmd-string t)))
    (message (format-volume-readout result))))

(defun vol-closure ()
  (let ((volume-level (get-volume)))
    (lambda (amnt &optional (set nil))
      "add amnt to volume level, and use amixer to set to the new volume"
      (if set
	  (progn (setf volume-level amnt)
		 (set-volume amnt))
	  (let ((newlevel (+ volume-level amnt)))
	    (when (< newlevel 100)
	      (setf volume-level newlevel)
	      (set-volume newlevel)))))))

(defparameter *volume-param* (vol-closure))

(defun inc-volume (amnt)
  (let ((*timeout-wait* 2))
    (funcall *volume-param* amnt)))
(defun force-volume (amnt)
  (let ((*timeout-wait* 2))
    (funcall *volume-param* amnt t)))

(defcommand change-volume-by (amnt) ((:number "Level to change volume by: "))
  (inc-volume amnt))
(defcommand re-set-volume (&optional amnt) ((:number "set volume to: "))
  (force-volume amnt))

;;; manage screen temperature

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

;;; manage brightness

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
						    :exit-on ((kbd "RET") (kbd "ESC")
							      (kbd "C-g")))
  ((kbd "SPC") "fnext")
  ((kbd "C-SPC") "fnext"))
