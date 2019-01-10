(in-package :stumpwm)

(defun find-sibling-left (frame-tree sibling-of &optional (cdr-is-nil? t))
  "this takes the frame tree and a frame to find the sibling of. it returns a sibling
 where sibling can be either a frame, 
or a list. it should not be nil. this only exists cause the sibling function included
in stumpwm seems to just go back to the 0th frame... so... the sibling is determined 
based on where the frame is in the tree. the frame tree is regular, except when theres
just one frame. eg:
one   frame '(frame)
many frames '(((frame (frame frame)) frame))
or  another '(((frame frame) (frame frame)))

its into a tree like so: (root-frame) is just one frame. then we split that frame,
so the frame becomes a list, containing frames -> ((frame frame)). so the length 
of the list will always be 1(one) at the top level, and two at every level below 
that. 

defun find-sibling-left '(frame-tree sibling-of &optional (cdr-is-nil? t))
"
  (when frame-tree
    (when cdr-is-nil?
      (setf frame-tree (car frame-tree)))
    (let ((left-branch-or-leaf (first frame-tree))
	  (right-branch-or-leaf (second frame-tree))
	 ;; check-res
	  )
      (cond ((listp left-branch-or-leaf)
	     (find-sibling-left left-branch-or-leaf sibling-of nil))
	    ((eq left-branch-or-leaf sibling-of)
	     right-branch-or-leaf) ;; return the sibling
	    ((eq right-branch-or-leaf sibling-of)
	     left-branch-or-leaf) ;; return the sibling
	    ((listp right-branch-or-leaf)
	     (find-sibling-left right-branch-or-leaf sibling-of nil))
	    (t
	     :t-clause))
      ;; (return-from find-sibling-left check-res)
      )))

(defun find-sibling-right (frame-tree sibling-of &optional (cdr-is-nil? t))
  (when frame-tree
    (when cdr-is-nil?
      (setf frame-tree (car frame-tree)))
    (let ((left-branch-or-leaf (first frame-tree))
	  (right-branch-or-leaf (second frame-tree))
	 ;; check-res
	  )
      (cond ((listp right-branch-or-leaf)
	     (find-sibling-right right-branch-or-leaf sibling-of nil))
	    ((eq right-branch-or-leaf sibling-of)
	     left-branch-or-leaf) ;; return the sibling
	    ((eq left-branch-or-leaf sibling-of)
	     right-branch-or-leaf) ;; return the sibling
	    ((listp left-branch-or-leaf)
	     (find-sibling-right left-branch-or-leaf sibling-of nil))
	    (t
	     :t-clause))
      ;; (return-from find-sibling-left check-res)
      )))

(defun find-sibling (frame-tree sibling-of &optional (cdr-is-nil? t))
  "this takes the frame tree and a frame to find the sibling of. it returns a sibling
 where sibling can be either a frame, 
or a list. it should not be nil. this only exists cause the sibling function included
in stumpwm seems to just go back to the 0th frame... so... the sibling is determined 
based on where the frame is in the tree. the frame tree is regular, except when theres
just one frame. eg:
one   frame '(frame)
many frames '(((frame (frame frame)) frame))
or  another '(((frame frame) (frame frame)))

its into a tree like so: (root-frame) is just one frame. then we split that frame,
so the frame becomes a list, containing frames -> ((frame frame)). so the length 
of the list will always be 1(one) at the top level, and two at every level below 
that. 

defun find-sibling-left '(frame-tree sibling-of &optional (cdr-is-nil? t))
"
  (let ((search-left (find-sibling-left frame-tree sibling-of cdr-is-nil?))
	(search-right (find-sibling-right frame-tree sibling-of cdr-is-nil?)))
    (if (eq search-left :t-clause)
	search-right
	search-left)))

(defun focus-sib (&optional (group (current-group)) (frame (current-frame)))
  (let ((sibling (find-sibling (tile-group-frame-tree group) frame)))
    (if (listp sibling)
	(curframe)
	(focus-frame group sibling))))

(defcommand sib () ()
  (focus-sib))


(defun remove-sib (&optional (group (current-group)) (frame (current-frame)))
  (let ((sibling-frame (find-sibling (tile-group-frame-tree group) frame)))
    (if (listp sibling-frame)
	(progn (remove-sib group (car sibling-frame))
	       (focus-frame group (car sibling-frame))
	       (run-commands "remove")
	       (curframe))
	(remove-split (current-group) sibling-frame))))

(defcommand remove-sibling () ()
  (remove-sib))
