(in-package :stumpwm)

;; we will run the command after hanging a self removing function from
;; the *focus-window-hook*. the hanging function  will send in the current window
;; alongside any provided arguments into the provided function.

;; example:
;; (with-open-window "Xfce4-terminal -e alsamixer" nil #'float-window (current-group))

(defparameter *with-window*
;  "function, arguments, class restrictor."
  '(nil nil nil))


;; could be stored as ((function args)(function args)(function args))
;; and then we call every function with cwin in our window-hanger

(defun with-win (cmd restrictor function &rest args)
  (push (list function args restrictor) *with-window-hook*)
  (add-hook *focus-window-hook* 'window-hanger)
  (if (stringp cmd)
      (run-shell-command cmd)
      (run-commands (format nil "~S" cmd))))

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

;;; Useful functions for this macro:
(defun re-splat-window (cwin &key (new-class nil) (new-title nil) (new-role nil))
  "this takes a window and new strings for the windows property slots. one thing,
is that some windows will re-splat themselves, f.eks. mousepad will reset its 
title to the title of the document opened - generally Untitled 1. "
  (when new-class
    (setf (window-class cwin) new-class))
  (when new-title
    (setf (window-title cwin) new-title))
  (when new-role
    (setf (window-role cwin) new-role)))

(defun reclassify-window (cwin new-class)
  (setf (window-class cwin) new-class))

(defun float-things (cwin)
  (float-window cwin (current-group)))

(defun float-in-tiles (cwin &key (always-show nil) (always-on-top nil)
			      (new-class nil) (x nil) (y nil) (width nil)
			      (height nil))
  (progn
    (focus-all cwin)
    (float-this)
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
    ))
