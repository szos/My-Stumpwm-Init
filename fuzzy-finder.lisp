(in-package :stumpwm)

(defun flatten-list (l)
  (if l
      (if (atom l)
	  (list l)
	  (mapcan #'flatten-list l))))

(defun window-matches-properties-fuzzy (window &key class instance type role title)
  "Returns T if window matches all the given properties"
  ;; (when (string= (window-group window) ".trash")
  ;;   (return-from window-matches-properties-fuzzy nil))
  (and
   (if (string= (group-name (window-group window)) ".trash") nil t)
   (if class (search (string-upcase class) (string-upcase (window-class window))) t)
   (if instance (search (string-upcase instance) (string-upcase (window-res window))) t)
   (if type (search (string-upcase type) (string-upcase (window-type window))) t)
   (if role (search (string-upcase role) (string-upcase (window-role window))) t)
   ;; (if role 
   ;;     (string-match (window-role window) role) t)
   (if title (search (string-upcase title) (string-upcase (window-title window))) t)
   t))

(defun find-matching-windows-fuzzy (props all-groups all-screens &optional
								   (groups-to-omit nil))
  "Returns list of windows containing @var{props}. eg if its passed 'h' 
all windows containing h in the property are listed @var{all-groups} 
will find windows on all groups. Same for @{all-screens}. Result is sorted 
by group and window number, with group being more significant (think radix sort)."
  (let* ((screens (if all-screens
                      *screen-list*
                      (list (current-screen))))
	 (groups (let ((glist (if all-groups
				  (flatten-list (mapcar #'screen-groups screens))
				  (list (current-group)))))
		   (mapcar (lambda (g)
			     (setf glist (remove g glist)))
			   groups-to-omit)
		   glist))
	 (winlist (mapcan #'group-windows groups))
         (matches (remove-if-not (lambda (w)
				   (apply 'window-matches-properties-fuzzy w props))
				 winlist)))
    (stable-sort (sort matches #'< :key #'window-number)
                 #'< :key (lambda (w) (group-number (window-group w))))))

(defun fuzzy-finder (&key
		       (props '(:class "") props-supplied-p)
		       (fmt *window-format*)
		       (all-groups *run-or-raise-all-groups*)
		       (all-screens *run-or-raise-all-screens*)
		       (groups-to-omit nil))
  "returns a window chosen by the user after selection from a windowlist
derived from the properties sent in. if no properties are sent in it defaults
to collecting all windows. if no windows are found returns nil, if one found 
returns the window, otherwise user selects window from menu. "
  (if props-supplied-p
      (let ((matches (flatten-list
                      (loop for x in props
			 collect (find-matching-windows-fuzzy x all-groups all-screens
							      groups-to-omit)))))
	(case (length matches)
          (0 (progn (message "Nothing Found...")
		    :not-found))
      	  (t (select-window-from-menu matches fmt))))
      (let ((matches (find-matching-windows-fuzzy props all-groups all-screens
						  groups-to-omit)))
	(case (length matches)
      	  (0 (progn (message "Nothing Found...")
		    :not-found))
      	  (t (select-window-from-menu matches fmt))))))
