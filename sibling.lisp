(in-package :stumpwm)

(defcommand (sib tile-group) (&key (group (current-group)) (frame (current-frame))
				   (which-sibling #'car)) ()
  (let ((sibling (closest-sibling
	  (list
	   (tile-group-frame-head group (frame-head group
						    frame)))
	  frame)))
    (if (listp sibling)
	(focus-frame group (funcall which-sibling sibling))
	;; (curframe)
	(focus-frame group sibling))))

;; (defun remove-split-sibling (tree &optional (group (current-group)))
;;   "this function takes a frame tree, and optionally the group that frame tree 
;; belongs to, if its not the current group. It then removes the frames in the 
;; tree. if a frame is split and contains a list of two frames rather than a frame,
;; we recurse, treating that split frame as the tree in our next stack frame. "
;;   (let ((left (car tree))
;; 	(right (second tree)))
;;     (when (listp left)
;;       (remove-split-sibling left group))
;;     (when (listp right)
;;       (remove-split-sibling right group))
;;     (when (frame-p left)
;;       (remove-split group left))
;;     (when (frame-p right)
;;       (remove-split group right))))

(defcommand (remove-sibling tile-group)
    (&optional (group (current-group))
	       (frame (let ((window-type (type-of (current-window))))
			(if (eq window-type 'float-window)
			    nil ;; (focus-all (current-window))
			    (window-frame (current-window))))))
    ()
  "removes the closest sibling frame to the specified frame, which defaults
to the current frame. if the closes sibling is split, we call 
remove-split-sibling, otherwise we just remove the sibling frame. "
  (when frame
    (labels ((remove-split-sibling (tree group)
	       (let ((left (car tree))
		     (right (second tree)))
		 (when (listp left)
		   (remove-split-sibling left group))
		 (when (listp right)
		   (remove-split-sibling right group))
		 (when (frame-p left)
		   (remove-split group left))
		 (when (frame-p right)
		   (remove-split group right)))))
      (let ((sibling
	     (closest-sibling
	      (list (tile-group-frame-head group (frame-head group frame)))
	      frame)))
	(cond ((listp sibling) ;; sibling is split
	       (remove-split-sibling sibling group))
	      ((frame-p sibling) ;; sibling is a frame
	       (remove-split group sibling))
	      (t ;; something went wrong, show the current frame.
	       (curframe)))))))
