(in-package :stumpwm)

;;; define a macro to launch an application and set its class

;;;;;;;;;;;;;;;;;;;;;;;
;;; Reclass Windows ;;;
;;;;;;;;;;;;;;;;;;;;;;;


;;; set up mini object system, wanted for controlling
;;; closures-as-objects
(defun calla (obji function &rest args)
  (cond ((and function (not args))
         (destructuring-bind (possible &rest rest) obji
           (destructuring-bind (name funct) possible
             (when (equal name function)
               (return-from calla (funcall funct))))
           (calla rest function)))
        ((and function args)
         (destructuring-bind (possible &rest rest) obji
           (destructuring-bind (name funct) possible
             (when (equal name function)
               (return-from calla (funcall funct args))))
           (calla rest function args)))
        ((and (not function) args)
         (funcall (cadar obji) args))
        (t
         (funcall (cadar obji)))))

(defmacro define-obji (name vars functs)
  "this macro takes a name, a variable section, and a function section.
a word on usage: members of vars can be functions, but cannot refer to 
other members of vars. members of functs must be functions, as they are
transformed into lambda functions. "
  (let ((funct-list (mapcar #'(lambda (cc)
                                (destructuring-bind (id params &rest body) cc
                                  `(list ',id (lambda ,params ,@body))))
                            functs)))
    `(defparameter ,name 
       (let ,vars
         (list ,@funct-list)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Reclass Windows ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-obji *reclass-closure*
    ((convert-from "")
     (convert-to ""))
  ((get-set-to (&optional (setter nil))
	       (if setter
		   (setf convert-to (car setter))
		   convert-to))
   (get-set-from (&optional (setter nil))
		 (if setter
		     (setf convert-from (car setter))
		     convert-from))))

(defun reset-reclass-closure ()
  (define-obji *reclass-closure*
    ((convert-from "")
     (convert-to ""))
  ((get-set-to (&optional (setter nil))
	       (if setter
		   (setf convert-to (car setter))
		   convert-to))
   (get-set-from (&optional (setter nil))
		 (if setter
		     (setf convert-from (car setter))
		     convert-from)))))

;; (reclass "cool-retro-term -e alsamixer" "Alsamxier")

(defmacro reclass (cmd class &optional (oldclass nil))
  `(progn
     (if ,oldclass
	 (calla *reclass-closure* 'get-set-from ,oldclass)
	 (calla *reclass-closure* 'get-set-from ""))
     (calla *reclass-closure* 'get-set-to ,class)
     (add-hook *focus-window-hook* 'self-remover)
     (run-shell-command ,cmd)))

(defun self-remover (cwin lwin)
  (declare (ignore lwin))
  (if (equal "" (car (calla *reclass-closure* 'get-set-from)))
      (progn
	(setf (window-class cwin) (calla *reclass-closure* 'get-set-to))
	(remove-hook *focus-window-hook* 'self-remover))
      (when (equal (window-class cwin)
		   (car (calla *reclass-closure* 'get-set-from)))
	(setf (window-class cwin) (calla *reclass-closure* 'get-set-to))
	(remove-hook *focus-window-hook* 'self-remover))))
