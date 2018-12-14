(in-package :stumpwm)

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
	    '(raise win))
       (with-open-window ,cmd ,restrictor
			#'(lambda (cwin)
			    ,@lambda-function-body)))))

(defun float-in-tiles (cwin &key (always-show nil) (always-on-top nil)
			      (new-class nil) (x nil) (y nil) (width nil)
			      (height nil))
  (progn
    (float-window cwin (window-group cwin))
    (float-window-move-resize cwin :x x :y y :width width :height height)
    (if new-class
	(setf (window-class cwin) (concatenate 'string "|FLOAT|" new-class))
	(setf (window-class cwin) (concatenate 'string "|FLOAT|" (window-class cwin))))
    ;; (unless (eq cwin (current-window))
    ;;   (raise cwin))
    (when always-show
      (toggle-always-show))
    (when always-on-top
      (toggle-always-on-top))))

(defun reclassify-window (cwin new-class)
  (setf (window-class cwin) new-class))



