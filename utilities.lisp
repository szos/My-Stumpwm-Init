(in-package :stumpwm)

(defmacro with-open-files (((stream filespec &rest options) &rest other-specs)
			   &body body)
  `(with-open-file (,stream ,filespec ,@options)
     ,@(if other-specs
	   `((with-open-files ,other-specs
	       ,@body))
	   body)))

(defmacro pif (test then &body else)
  `(if ,test ,then (progn ,@else)))

(defun unix-cat (file)
  (string-trim '(#\newline) 
	       (with-output-to-string (s)
		 (with-open-file (stream file)
		   (loop for l = (read-line stream nil)
			 until (null l)
			 do (format s "~a~%" l))))))

(defun get-memory-usage-percent ()
  (let (total available)
    (with-open-file (stream "/proc/meminfo")
      (block memblock
	(loop for l = (read-line stream nil)
	      when (null l)
		return nil
	      do (let ((split (cl-ppcre:split ":" l)))
		   (cond ((string= (car split) "MemTotal")
			  (setf total
				(parse-integer
				 (string-trim '(#\k #\b #\space #\B)
					      (cadr split)))))
			 ((string= (car split) "MemAvailable")
			  (setf available
				(parse-integer
				 (string-trim '(#\k #\b #\space #\B)
					      (cadr split))))))
		   (when (and total available)
		     (return-from memblock))))))
    (* 100 (/ (- total available) total))))

(defun pull-to-group (window &optional (group (current-group)))
  "pulls window to group and focuses it."
  (pull-w window group)
  (group-focus-window group window))

(defun run-raise-pull-list (cmd props &key prompt
					(all-groups *run-or-raise-all-groups*)
					(all-screens *run-or-raise-all-screens*)
					(filter-pred *window-menu-filter*)
					(fmt *window-format*)
					remove-props)
  "run-raise-pull-list opens a menu to choose either an existing window matching
one of the properties pased in with props, or either run the shell command 
specified by cmd or select a shell command to run if cmd is a list¹. If 
remove-props is provided any windows matching those properties will be removed 
from the list. This can be useful when dealing with browsers, for example.
¹cmd can be formatted one of three ways: as a shell command, as a list of shell
commands, or as a list of lists of display texts and associated shell commands. 
examples: 
\"firefox\", 
'(\"firefox\" \"palemoon\"), or 
'((\"Firefox\" \"firefox\")
  (\"Pale Moon\" \"palemoon\")) "
  (let ((windows
	  (flatten
	   (loop for prop in (if (keywordp (car props)) (list props) props)
		 collect (find-matching-windows prop all-groups all-screens))))
	(remove
	  (flatten
	   (loop for prop in (if (and remove-props (keywordp (car remove-props)))
				 (list remove-props) remove-props)
		 collect (find-matching-windows prop all-groups all-screens)))))
    (loop for window in remove do (setf windows (remove window windows)))
    (if (not windows)
	(if (stringp cmd)
	    (run-shell-command cmd)
	    (run-shell-command
	     (let ((res (select-from-menu (current-screen) cmd "Launch: ")))
	       (if (= 1 (length res)) (car res) (cadr res)))))
	(let* ((table  `(("LAUNCH" ,cmd)
			 ,@(mapcar (lambda (el)
				     (list (format-expand
					    *window-formatters* fmt el)
					   el))
				   windows)))
	       (result (second (select-from-menu (current-screen) table prompt
						 1 nil filter-pred))))
	  (cond ((not result) '())
		((stringp result) (run-shell-command result))
		((listp result)
		 (run-shell-command
		  (let ((res (select-from-menu (current-screen) cmd "Launch: ")))
		    (if (= 1 (length res)) (car res) (cadr res)))))
		((window-visible-p result)
		 (if (eq (window-group result) (current-group))
		     (group-focus-window (current-group) result)
		     (pull-to-group result)))
		(t 
		 (if (eq (window-group result) (current-group))
		     (group-focus-window (current-group) result)
		     (pull-to-group result))))))))

(defcommand duckduckgo (search) ((:rest "Search: "))
  (nsubstitute #\+ #\space search)
  (run-shell-command (concatenate 'string "firefox https://duckduckgo.com/?q=" search)))

