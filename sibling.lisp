(in-package :stumpwm)

(defun focus-sib (&optional (group (current-group)) (frame (current-frame)))
  (let ((sibling (closest-sibling
	  (list
	   (tile-group-frame-head group (frame-head group
						    frame)))
	  frame)))
    (if (listp sibling)
	(curframe)
	(focus-frame group sibling))))

(defcommand sib () ()
  (focus-sib))

(defun remove-split-sibling (tree &optional (group (current-group)))
  (let ((left (car tree))
	(right (second tree)))
    (when (listp left)
      (remove-split-sibling left group))
    (when (listp right)
      (remove-split-sibling right group))
    (when (frame-p left)
      (remove-split group left))
    (when (frame-p right)
      (remove-split group right))))

(defcommand (remove-sibling tile-group)
    (&optional (group (current-group))
	       (frame (current-frame)))
    ()
  "removes the closest sibling frame to the specified frame, which defaults
to the current frame (requires current-frame function). if the closes sibling
is split, we go into it and remove every split up through the sibling frame, 
thus removing every frame in the sibling tree. "
  (let ((sibling
	 (closest-sibling
	  (list
	   (tile-group-frame-head group (frame-head group
						    frame)))
	  frame)))
    (cond ((listp sibling)
	   ;; sibling is split
	   (remove-split-sibling sibling group))
	  ((frame-p sibling)
	   ;; sibling is a frame
	   (remove-split group sibling))
	  (t
	   nil))))
