(in-package :stumpwm)

(define-condition in-as-error (error)
  ((both :accessor :in-and-as
	 :initarg :both
	 :initform nil
	 :documentation "Indicates that both IN and AS were used in a for statement.")
   (neither :accessor :in-nor-as
	    :initarg :neither
	    :initform nil
	    :documentation "Indicates that neither IN nor AS were used in a for statement. ")
   (for-statement :accessor :for-statement
		  :initarg :statement-information
		  :initform nil
		  :documentation "The for statement, as it was sent in, for use in restarts. ")))



;; (for (x :in '(0 1 2 3 4 5) :accum-in (acc 0))
;;   (print x)
;;   (setf acc (+ acc x)))

;; (for ('(1 2 3 4 5) :as x :accum-in (acc 0))
;;   (print x)
;;   (setf acc (+ x acc)))


(defmacro for ((thing &key (in nil) (accum-in (gensym))
			(as nil))
	       &body body)
  "this macro creates a for loop, as in c or python or what have you. "
  (cond ((and as (not in))
	 `(let (,accum-in)
	    (mapcar #'(lambda (,as)
			,@body)
		    ,thing)
	    ,(if (listp accum-in)
		 (car accum-in)
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
	 `(with-simple-restart (provide-in "proveide a :in variable" x-variable)
	    
	    (error 'in-as-error
		   :neither t :statement-information '(for (,thing ,(when accum-in
								      accum-in))
						       ,@body))))))

(defmacro define-keymap (&body bindings)
  "takes a list of bindings, in the form ((kbd \"key\") \"command\")
and creates a keymap with them, returning said keymap. "
  (let* ((m (gensym))
	 (bind-to-m (mapcar #'(lambda (bind)
				`(define-key ,m ,@bind))
			    bindings)))
    `(let ((,m (make-sparse-keymap)))
       ,@bind-to-m
       ,m)))

(defparameter *test-hydra*
  (define-keymap
    ((kbd "n") "meta C-n")))

(undefine-key *root-map* (kbd "C-S"))
(undefine-key *root-map* (kbd "S"))

(defcommand smart-split () ()
  (let* ((frame (current-frame))
	 (x1 (frame-x frame))
	 (x2 (frame-r frame))
	 (y1 (frame-y frame))
	 (y2 (frame-b frame))
	 ;; (ratio '((x 540) (y 960)))
	 )
    ;; ratio to keep at w/ mode line on is x=540 y=960
    ;; ratio w/o mode line is: the same... stump doesnt count the mode line when dealing with frames. 
    (cond ((> (- x2 x1) (- y2 y1))
	   (hsplit))
	  ((< (- x2 x1) (- y2 y1))
	   (vsplit))
	  (t
	   (message "square window, i dont know what to do. ")))))

(define-key *root-map* (kbd "C-M-s")
  (define-keymap
    ((kbd "v") "hsplit")
    ((kbd "V") "vsplit-equally")
    ((kbd "h") "vsplit")
    ((kbd "H") "hsplit-equally")))

(define-key *root-map* (kbd "M-s") "vsplit")
(define-key *root-map* (kbd "C-s") "hsplit")
(define-key *root-map* (kbd "s") "smart-split")

(defcommand mesage (str) ((:string "message: "))
  (message str))

;; (define-key *root-map* (kbd "C-z
;; C-f") "mesage hi")
;; this doesnt work...

(define-key *root-map* (kbd "M-b") "move-focus left")
(define-key *root-map* (kbd "M-f") "move-focus right")
(define-key *root-map* (kbd "M-n") "move-focus down")
(define-key *root-map* (kbd "M-p") "move-focus up")

(define-key *root-map* (kbd "s-p") "exchange-direction up")
(define-key *root-map* (kbd "s-n") "exchange-direction down")
(define-key *root-map* (kbd "s-b") "exchange-direction left")
(define-key *root-map* (kbd "s-f") "exchange-direction right")

(define-key *root-map* (kbd "C-f") "move-focus right")
(define-key *root-map* (kbd "C-b") "move-focus left")
(define-key *root-map* (kbd "C-n") "pull-hidden-next")
(define-key *root-map* (kbd "C-p") "pull-hidden-previous")

(define-key *root-map* (kbd "m") "mode-line")
(define-key *root-map* (kbd "r") "remove")
(define-key *root-map* (kbd "R") "remove-sibling")
(define-key *root-map* (kbd "M-r") "remove-sibling")
(define-key *root-map* (kbd "SPC") "change-frames")
(define-key *root-map* (kbd "O") "sib")

(define-key *root-map* (kbd "C-M-f") "gnext")
(define-key *root-map* (kbd "C-M-b") "gprev")

(define-key *root-map* (kbd "C-d") "describe-key")
(define-key *root-map* (kbd "'") "pullstr")
(define-key *root-map* (kbd ";") "colon")
(define-key *root-map* (kbd "w") "windowlist")

(define-key *root-map* (kbd "M-s-H-h") "ff-focus-search-bar") 

(define-key *top-map* (kbd "M-F1") "volume-set 0")
(define-key *top-map* (kbd "M-F2") "change-volume-by -5")
(define-key *top-map* (kbd "M-F3") "change-volume-by 5")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume -5")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume 5")
(define-key *top-map* (kbd "XF86AudioMute") "volume-set 0")

(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-increment -5")
(define-key *top-map* (kbd "C-XF86MonBrightnessDown") "brightness-set 0")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-increment 5")
(define-key *top-map* (kbd "C-XF86MonBrightnessUp") "brightness-set 100")

;;; block out function key functionality for doing other things. 
(defcommand test-msg () ()
  (message "registered function key"))

(define-key *top-map* (kbd "F1") "volume-set 0")
(define-key *top-map* (kbd "F2") "volume -5")
(define-key *top-map* (kbd "C-F2") "volume-set 50")
(define-key *top-map* (kbd "F3") "volume 5")
(define-key *top-map* (kbd "C-F3") "volume-set 100")
;;(define-key *top-map* (kbd "F4") "test-msg")
;;(undefine-key *top-map* (kbd "F4") )
;;(define-key *top-map* (kbd "F5") "test-msg")
(undefine-key *top-map* (kbd "F6"))
(define-key *top-map* (kbd "F7") "test-msg")
(define-key *top-map* (kbd "F8") "test-msg")
(define-key *top-map* (kbd "F9") "test-msg")
;; (undefine-key *top-map* (kbd "F10"); "test-msg"
;; 	      )
(define-key *top-map* (kbd "F11") "brightness-increment -5")
(define-key *top-map* (kbd "F12") "brightness-increment 5")

(undefine-key *root-map* (kbd "c"))

(defun remove-http/s (str)
  "takes a url, and returns the url without the http/s://"
  (if (string= (subseq str 0 5) "https")
      (subseq str 8)
      (subseq str 7)))

(defun youtube? (str)
  "is this site youtube?"
  (string= (subseq str 0 11) "www.youtube"))

(defun ddg? (str)
  "is this site duckduckgo?"
  (string= (subseq str 0 10) "duckduckgo"))

(defun invidio.us? (str)
  "is this site invidio.us?"
  (string= (subseq str 0 10) "invidio.us"))

(defcommand ff-focus-search-bar () ()
  (run-commands
   "meta C-l"
   "meta C-c")
  (let ((x-sel (remove-http/s (get-x-selection))))
    (cond ((youtube? x-sel)
	   (run-commands
	    "meta TAB"
	    "meta TAB"
	    "meta TAB"
	    "meta TAB"))
	  ((or (ddg? x-sel) (invidio.us? x-sel))
	   (run-commands
	    "meta TAB"
	    "meta TAB"
	    "meta TAB"))
	  ((invidio.us? x-sel)
	   (run-commands
	    "meta TAB"
	    "meta TAB"
	    "meta TAB"))
	  (t
	   nil))))

(defun url-p (url)
  (when (> (length url) 6)
    (or (string= (subseq url 0 7) "http://")
	    (string= (subseq url 0 8) "https://"))))

(defcommand watch-video () ()
  "use this command when on youtube, or another site that mpv can play from, 
and it will open the video in mpv."
  (let* ((curwin (current-window))
	 (url (progn (run-commands "meta C-c")
		     (get-x-selection))))
    (when (string= (window-class curwin) "Firefox")
      (unless (url-p url)
	(run-commands "meta F6"
		      "meta C-c")
	(setf url (get-x-selection)))
      (message "Opening video:  ~S" url)
      ;; (message "win: ~A.   url: ~A" curwin (get-x-selection))
      (run-shell-command (format nil "mpv ~A" url)))))

(define-remapped-keys
    `(("Firefox"
       ("C-g" . "ESC")

       ("C-v" . "SunPageDown")
       ("M-v" . "SunPageUp")
       ("M-<" . "Home")
       ("M->" . "End")
       
       ("C-n" . "Down")
       ("C-p" . "Up")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("M-f" . "C-Right")
       ("M-b" . "C-Left")
       ("M-DEL" . "C-DEL")

       ("C-e" . "End")
       ("C-a" . "Home")

       ("H-s" . "C-f")
       ("C-s" . "C-g")
       ("C-r" . "C-G")
       ("M-s" . "'")
       ("M-S" . ("C-;" "M-s-H-h"))
       
       ("C-w" . "C-x")
       ("M-w" . "C-c")
       ("C-y" . "C-v")

       ("M-F" . "C-TAB")
       ("M-B" . "C-S-TAB")
       ("H-b" . "C-S-SunPageUp")
       ("H-f" . "C-S-SunPageDown")

       ("C-B" . "C-[")
       ("C-F" . "C-]")
       
       ("C-k" . "C-w")
       ("C-u" . "C-T")
       ("C-N" . "C-n")
       ("H-n" . "C-n")
       ;; ("C-x" . ,(define-keymap
       ;; 		     ((kbd "u") "meta C-z")))
       )
      ("Riot"
       ("C-n" . "Down")
       ("C-p" . "Up")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("M-f" . "C-Right")
       ("M-b" . "C-Left")
       ("M-DEL" . "C-DEL")
       ("C-d" . "Delete")
       ("M-d" . "C-Delete")
       ("C-k" . ("S-End" "Delete"))

       ("C-e" . "End")
       ("C-a" . "Home")

       ("C-w" . "C-x")
       ("M-w" . "C-c")
       ("C-y" . "C-v"))
      ("Signal"
       ("C-n" . "Down")
       ("C-p" . "Up")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("M-f" . "C-Right")
       ("M-b" . "C-Left")
       ("M-DEL" . "C-DEL")
       ("C-d" . "Delete")
       ("M-d" . "C-Delete")
       ("C-k" . ("S-End" "Delete"))

       ("C-e" . "End")
       ("C-a" . "Home")

       ("C-w" . "C-x")
       ("M-w" . "C-c")
       ("C-y" . "C-v"))
      ("calibre"
       ("C-p" . "Up")
       ("C-n" . "Down")
       ("C-l" . "C-f"))
      (,(lambda (win) ;; for the inbox
	  (search "Mozilla Thunderbird" (window-title win)))
	("C-n" . "Down")
	("C-p" . "Up")
	("C-m" . "C-n") ;; new email
	("C-k" . "C-w")
	("C-K" . "C-T")
	("C-RET" . ("F8" "F6"))
	("M-o" . "F6")
	("M-O" . "S-F6")

	("C-w" . "C-x")
	("M-w" . "C-c")
	("C-y" . "C-v")

	("C-s" . "C-g")
	("C-r" . "C-G")
	("H-s" . "C-f")
	("C-g" . "ESC")

	("M-r" . "C-r")
	("M-R" . "C-R")

	("M-<" . "Home")
	("M->" . "End")
	
	("M-F" . "C-TAB")
	("M-B" . "C-S-TAB")

	("C-v" . "SunPageDown")
	("M-v" . "SunPageUp"))
      (,(lambda (win) ;; for composing messages
	  (and (search "Write:" (window-title win))
	       (search "- Thunderbird" (window-title win))))
	("C-n" . "Down")
	("C-p" . "Up")
	("C-f" . "Right")
	("C-b" . "Left")
	("C-a" . "Home")
	("C-e" . "End")

	("C-w" . "C-x")
	("M-w" . "C-c")
	("C-y" . "C-v")
	
	("C-s" . "C-f")
	("M-f" . "C-Right")
	("M-b" . "C-Left")
	("C-d" . "Delete")
	("C-g" . "ESC"))
      ("mpv"
       ("C-f" . "Right") ;; navigation / skip
       ("C-b" . "Left")
       ("M-f" . "Up")
       ("M-b" . "Down")
       ("C-p" . "0") ;; volume increase/decrease
       ("C-n" . "9"))))
