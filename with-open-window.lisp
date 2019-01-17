(in-package :stumpwm)

(defparameter *float-win-associated-tile* NIL)

(defparameter *with-window*
  '(nil nil nil)
  "function, arguments, class restrictor.")

(defun with-open-window (cmd restrict-class function &rest args)
  "stores the {function}, {args}, and {restrict-class} variables in a dynamic
variable so that with-window-hanger can grab them. it then hangs 
with-window-hanger on focus-window-hook. then it checks if {cmd} is a string, 
in which case its a shell command and is run. otherwise its treated as a 
stumpwm command (or list of commands) and run that way."
  (progn
    (setf (first *with-window*) function)
    (setf (second *with-window*) args)
    (setf (third *with-window*) restrict-class)
    (add-hook *focus-window-hook* 'with-window-hanger)
    (if (stringp cmd)
	(run-shell-command cmd)
	(if (cdr cmd)
	    (reduce #'run-commands cmd)
	    (funcall #'run-commands (car cmd))))))

(defun with-window-hanger (cwin lwin)
  "this gets hung on focus-window-hook. it will call the function with {cwin}
and calls the function, and then removes itself from the hook. update: added
protection against bad functions that error via unwind-protect"
  (declare (ignore lwin))
  (let ((func (first *with-window*))
	(args (second *with-window*))
	(restrictor (third *with-window*)))
    (when (or (not restrictor) (equal restrictor (window-class cwin)))
      (unwind-protect
	   (if args
	       (reduce func (cons cwin args))
	       (funcall func cwin))
	(remove-hook *focus-window-hook* 'with-window-hanger)))))

(defmacro define-open-window-command (name (&rest arg-names)
					      (&rest args)
						 class-to-search
						 pull?
						 cmd restrictor &body lambda-function-body)
  "This generates a command to either raise/pull a window or open it via the 
with-open-window function. This takes args the same way as defcommand. You can 
use cwin in the lambda-function-body to reference the window selected. This is 
an intentional variable capture. This depends on the fuzzy-finder function."
  `(defcommand ,name ,arg-names ,args
     (if-let ((win (fuzzy-finder '((:class ,class-to-search)))))
       ,(if (equal :pull pull?)
	    '(pull win)
	    '(focus-all win))
       (with-open-window ,cmd ,restrictor
			#'(lambda (cwin)
			    ,@lambda-function-body)))))
(defmacro define-open-win-com (name (&optional check-for-existing-windows? ;; nil
				     with-atribute
				     to-search &body if-found)
				       cmd restrictor &body lambda-body)
  "this macro creates a StumpWM command called name, with no arguments. 
if you dont pass in anything for check-for-existing-windows? and its associates
then it will be a 'one shot' thing. ie every time its run it will execute the
with-open-window element. OTHERWISE: pass t to check-for-existing-windows? to 
check for a window via fuzzy-finder. then pass to with-atribute an attribute 
such as :class or :title, and pass to to-search the string to search for. 
pass to if-found what to do if the window is found. you can reference the 
window with the keyword win. if the window isnt found, the with-open-window
form is executed. this form gets passed cmd, restrictor, and lambda-body. cmd
gets called, or if its a string its executed. restrictor is a class to restrict
to - the functions wont be called on the window unless it matches a specific 
class. the lambda body gets passed a list that is the body of the lambda. the
lambda takes one argument, the window, which can be referenced with cwin. 

this macro transforms this:
;; (define-open-win-com name (:check-for-existing-windows? t
;;                            :with-atribute :class :to-search \"search term\"
;;                            :if-found (focus-all win))
;;                           command restrictor (float-in-tiles cwin) 
;;                                              (reclassify-window cwin \"new-class\"))
turns into 
;; (defcommand name () ()
;;   (let ((win (fuzzy-finder '((:class \"search term\")))))
;;     (if (eq win :not-found)
;;         (with-open-window command restrictor 
;;                           #'(lambda (cwin)
;;                               (float-in-tiles cwin)
;;                               (reclassify-window cwin \"new-class\")))
;;         (focus-all win))))
 "
  `(defcommand ,name () ()
     ,(if check-for-existing-windows?
	  `(let ((win (fuzzy-finder '((,with-atribute ,to-search)))))
	     (if (eq win :not-found)
		 (with-open-window ,cmd ,restrictor
				   #'(lambda (cwin)
				       ,@lambda-body))
		 ,@if-found))
	  `(with-open-window ,cmd ,restrictor
			       #'(lambda (cwin)
				   ,@lambda-body)))))

(define-open-win-com test-xf (t :class "|FLOAT|tester" (focus-all win))
    "xfce4-terminal" nil
  (float-in-tiles cwin :new-class "tester"))

(defun float-in-tiles (cwin &key (new-class nil) (x-override nil) (y-override nil)
			      (width nil) (height nil))
  (float-window cwin (window-group cwin))
  (float-window-move-resize cwin :x x-override :y y-override :width width :height height)
  (if new-class
	(setf (window-class cwin) (concatenate 'string "|FLOAT|" new-class))
	(setf (window-class cwin) (concatenate 'string "|FLOAT|" (window-class cwin)))))

(defun reclassify-window (cwin new-class)
  (setf (window-class cwin) new-class))

(defun sq (num)
  "squares the argument"
  (* num num))

(defun find-closest-frame (window &key (preferred-axis :x))
  "this takes a window, grabs the x, y, width, and height of the window,
then it compares that with the x y width and height of every frame in the 
windows current group. whichever is closest to the window, is the frame the
window gets put into. "
  (let* ((win-x1 (window-x window))
	 (win-y1 (window-y window))
	 ;; (win-x2 (+ win-x1 (window-width window)))
	 ;; (win-y2 (+ win-y1 (window-height window)))
	 (framelist (group-frames (window-group window)))
	 (w/f-distance-x '(10000 nil))
	 (w/f-distance-y '(10000 nil)))
    ;; (if (= win-x1 ))
    (loop for frame in framelist
       do (let* (;; (framex1 (frame-x frame))
		 ;; (framey1 (frame-y frame))
		 ;; (framex2 (frame-r frame))
		 ;; (framey2 (frame-b frame))
		 ;; (frame-center-x (- framex2 (/ (- framex2 framex1) 2)))
		 ;; (frame-center-y (- framey2 (/ (- framey2 framey1) 2)))
		 (dfx (abs (- win-x1 (frame-x frame))))
		 (dfy (abs (- win-y1 (frame-y frame)))))
	    ;; (cond (()))
	    (when (< dfx (car w/f-distance-x))
	      (setf w/f-distance-x `(,dfx ,frame)))
	    (when (< dfy (car w/f-distance-y))
	      (setf w/f-distance-y `(,dfy ,frame)))))
    (cond ((eq (second w/f-distance-x) (second w/f-distance-y))
	   (second w/f-distance-x))
	  ((eq preferred-axis :x)
	   (second w/f-distance-x))
	  ((eq preferred-axis :y)
	   (second w/f-distance-y))
	  (t
	   nil))))

(defun unfloat-window-to-frame (window &key (frame (find-closest-frame window))
					 (group (window-group window)))
  (change-class window 'tile-window :frame frame)
  (setf (window-frame window) frame
	(frame-window frame) window
	(tile-group-current-frame (window-group window)) frame)
  (update-decoration window)
  (sync-frame-windows group frame)
  (when (and (> (length (window-class window)) 7)
	     (string= (subseq (window-class window) 0 7) "|FLOAT|"))
    (reclassify-window window (subseq (window-class window) 7))))

(defcommand unfloat-current-window () ()
  (unfloat-window-to-frame (current-window)))

(defcommand float-current-window () ()
  (float-in-tiles (current-window)))

(defcommand float-curwin () ()
  (let ((win (current-window)))
    (setf *float-win-associated-tile* (cons `((,win . ,(window-frame win)))
					    *float-win-associated-tile*))
    (float-in-tiles win)))

;; (defcommand unfloat-curwin () ()
;;   (let ((win (current-window))
;; 	(new-win-til-assoc nil))
;;     (loop for win.frame in *float-win-associated-tile*
;;        do (if (eq (caar win.frame) (current-window))
;; 	      (unfloat-window-to-frame (caar win.frame) :frame (cdar win.frame))
;; 	      (cons win.frame new-win-til-assoc)))))

(defcommand access-floats () ()
  "looks for windows floated with the (with-open-window... #'float-in-tiles)"
  (let* ((win (fuzzy-finder '((:class "|FLOAT|")) *window-format* nil nil))
	 (action (cadr (select-from-menu (current-screen)
					 '(;; ("raise" :raise)
					   ("focus" :focus)
					   ("delete" :delete))))))
    (unless (equalp win :not-found)
      (cond ((eq action :focus)
	     (focus-all win)
	     (message "foocus"))
	    ((eq action :delete)
	     (message "delete")
	     (delete-window win))
	    (t
	     (message "unknown action:  ~A" action))))))

(defmacro rfd-win (screen win)
  `(cadr (select-from-menu ,screen
			   '(("raise" (raise ,win))
				      ("focus" (focus-all ,win))
				      ("delete" (delete-window ,win))))))
