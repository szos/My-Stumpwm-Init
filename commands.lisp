(in-package :stumpwm)

(define-stumpwm-type :whole-string (input prompt)
  (or (argument-pop-rest input)
      (read-one-line (current-screen) prompt)))

(defcommand setxkbmap (language &optional options)
    ((:string "language: ")
     (:whole-string "Options: "))
  (run-shell-command (format nil "setxkbmap ~a -option ~a"
			     language (or options ""))))

(defcommand xbacklight (percentage) ((:number "set backlight percentage:  "))
  (run-shell-command (format nil "xbacklight -set ~s" percentage)))

(defcommand redshift (temp) ((:number "Enter a temperature"))
  (if (= temp 0)
      (run-shell-command "redshift -x")
      (run-shell-command (format nil "redshift -x && redshift -O ~a" temp))))

(defcommand daytime () ()
  (redshift 0)
  (xbacklight 100))

(defcommand nighttime () ()
  (redshift 3000)
  (xbacklight 10))

(defcommand file-manager (&optional command) ((:string))
  (let ((managers '(("XFE" "xfe")
		    ("Common Lisp File Manager" "clfm"))))
    (if command
	(run-shell-command command)
	(run-raise-pull-list managers '((:class "Clfm")
					(:class "Xfe"))))))

(defcommand browser (&optional browser-command) ((:string))
  (let ((browsers '(("Pale Moon" "palemoon") ("Firefox" "firefox")
		    ("Dillo" "dillo") ("W3M" "xterm -e w3m duckduckgo.com")
		    ("Pale Moon Private" "palemoon --private-window")
		    ("Firefox Private" "firefox --private-window"))))
    (if browser-command
	(run-shell-command browser-command)
	(run-raise-pull-list browsers '(:role "browser")
			     :remove-props '(:role "browser-window")))))

(defcommand vlc () ()
  (run-raise-pull-list "vlc" '(:class "vlc")))

(let ((ratwarper-running nil))
  (defcommand toggle-jiggle () ()
    (if ratwarper-running
	(progn (run-shell-command "killall ratwarper.sh")
	       (setf ratwarper-running nil)
	       (message "Ratwarper was stopped"))
	(progn (run-shell-command "ratwarper.sh")
	       (setf ratwarper-running t)
	       (message "Ratwarper was started")))))






