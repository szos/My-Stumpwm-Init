(in-package :stumpwm)

(defmacro format-shell-command (control-string &body format-stuff)
  "runs the shell command through format, with"
  `(run-shell-command (format nil ,control-string ,@(when format-stuff
						      format-stuff))))

(define-syntax scheme-while 
    (syntax-rules ()
		  ((scheme-while cond body ***)
		   (loop while cond do body ***))))

(defmacro while (cond &body body)
  `(loop while ,cond
	do ,@body))

(defmacro cfor ((bindings test change) &body body)
  "approximation of the for statement found in c. an example call would be:
;; (cfor (((x 0)) (< x 5) (incf x))
;;   (princ x)) 
==> 01234"
  `(let ,bindings
     (loop while (not ,test)
	do ,@body
	  ,change)))

(defmacro for ((thing &key (in nil) (accum-in (gensym))
		      (as nil))
	       &body body)
  "this macro creates a for loop, as in c or python or what have you. 
there are several ways this can be used. please note that :accum-in is optional
the general usage is 
;;(for (initialization) (forms))

;; (for (x :in '(1 2 3 4))
;;   (princ x))
=> 1234
;; (for (x :in '(1 2 3 4) :accum-in y;)
;;   (princ x)
;;   (setf y (+ y x)))
=> 1234
10
;; (for ('(1 2 3 4) :as m)
;;   (princ m))
=> 1234
;; (for (x :in '(1 2 3 4) :as y :accum-in (z 0))
;;   (princ x)
;;   (princ y)
;;   (princ z)
;;   (setf z (+ z x y)))
=> 1102223364412
20
;; (for ('1 2 3 4 :accum-in y)
;;   (if y
;;       (setf y
;;       (setf
;; (for (5)
;;   (princ 0))
=> 0000
"
  (cond ((and (numberp thing) (not in) (not as))
	 `(loop for ,(gensym) from 0 to ,thing
	       do ,@body))
	((and as (not in))
	 `(let (,accum-in)
	    (mapcar #'(lambda (,as)
			,@body)
		    ,thing)
	    ,(when accum-in
	       accum-in)))
	((and in (not as))
	 `(let (,accum-in)
	    (mapcar #'(lambda (,thing)
			,@body)
		    ,in)
	    ,(if (listp accum-in)
		 (car accum-in)
		 accum-in)))
	((and in as)
	 `(let (,accum-in
		,as)
	    (mapcar #'(lambda (,thing)
			(setf ,as ,thing)
			,@body)
		    ,in)))
	(t
	 `nil)))
