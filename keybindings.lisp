(in-package :stumpwm)

(defmacro define-hydra (&body bindings)
  (let* ((m (gensym))
	 (bind-to-m (mapcar #'(lambda (bind)
				(push m bind)
				(push 'define-key bind))
			    bindings)))
    `(let ((,m (make-sparse-keymap)))
       ,@bind-to-m
       ,m)))

(defparameter *test-hydra*
  (define-hydra
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
  (define-hydra
    ((kbd "v") "hsplit")
    ((kbd "V") "vsplit-equally")
    ((kbd "h") "vsplit")
    ((kbd "H") "hsplit-equally")))

(define-key *root-map* (kbd "M-s") "vsplit")
(define-key *root-map* (kbd "C-s") "hsplit")
(define-key *root-map* (kbd "s") "smart-split")

(define-key *root-map* (kbd "M-b") "move-focus left")
(define-key *root-map* (kbd "M-f") "move-focus right")
(define-key *root-map* (kbd "M-n") "move-focus down")
(define-key *root-map* (kbd "M-p") "move-focus up")

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

;;; this is for define-remapped-keys; it allows us to call functions.
(define-key *root-map* (kbd "M-s-H-h") "ff-focus-search-bar") 

(define-key *top-map* (kbd "M-F1") "volume-set 0")
(define-key *top-map* (kbd "M-F2") "volume -5")
(define-key *top-map* (kbd "M-F3") "volume 5")


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
(define-key *top-map* (kbd "F3") "volume 5")
;;(define-key *top-map* (kbd "F4") "test-msg")
;;(undefine-key *top-map* (kbd "F4") )
;;(define-key *top-map* (kbd "F5") "test-msg")
(undefine-key *top-map* (kbd "F6"))
(define-key *top-map* (kbd "F7") "test-msg")
(define-key *top-map* (kbd "F8") "test-msg")
(define-key *top-map* (kbd "F9") "test-msg")
(undefine-key *top-map* (kbd "F10"); "test-msg"
	      )
(define-key *top-map* (kbd "F11") "brightness-increment -5")
(define-key *top-map* (kbd "F12") "brightness-increment 5")

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
       ("H-n" . "C-n"))
      ("Riot"
       ("C-n" . "Down")
       ("C-p" . "Up")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("M-f" . "C-Right")
       ("M-b" . "C-Left")
       ("M-DEL" . "C-DEL")

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
