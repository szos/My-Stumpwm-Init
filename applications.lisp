(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;
;;; APPLICATIONS ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; WEB BROWSERS ;;;
;;;;;;;;;;;;;;;;;;;;

(defcommand web-browser () ()
  (tor))

;;; NEXT
(defcommand next-browser () ()
  (run-shell-command "firejail next")
  ;;(run-shell-command "xterm -e sbcl --load /home/shos/next-0.08/next/start-next.lisp")
  )

(defcommand next-browser-force-exit () ()
  (run-shell-command "killall next")
  (run-shell-command "killall next-gtk-webkit"))

;;; TOR
(defcommand tor () ()
  (run-raise-or-pull "./TOR/Browser/start-tor-browser" '(:class "Tor Browser")))

;;; FIREFOX
(defcommand firefox () ()
  "run firefox or set focus to it f already running"
  (run-raise-or-pull "firejail firefox" '(:class "firefox")))

(defcommand i2p-firefox () ()
  "run firefox with the i2p profile. user must set up profile before use.
this only allows one instance of the I2P profile. to make new windows, use C-n.
remeber to start the service from sysctl!"
  (let ((active?
	 (string=
	  (run-shell-command "systemctl status i2prouter.service | grep -o \"active (running)\"" t)
	  "active (running)
")))
    (unless active?
      (message "starting i2p router")
      (run-shell-command "systemctl start i2prouter.service"))
    ;; (run-raise-or-pull "firejail firefox -new-instance -P I2P -url 127.0.0.1:7657/home"
    ;; 		       '(:class "Firefox"))
    (run-raise-or-pull (with-open-window "firejail firefox -new-instance -P I2P -url 127.0.0.1:7657/home"
    			 "Firefox" #'reclassify-window "I2P")
    		       '(:class "I2P"))))

(defcommand i2p-stop () ()
  (let  ((active?
	 (string=
	  (run-shell-command "systemctl status i2prouter.service | grep -o \"active (running)\"" t)
	  "active (running)
")))
    (when active?
      (message "stopping i2p router")
      (run-shell-command "systemctl stop i2prouter.service"))))

(defcommand firefox-n () ()
  "run firefox"
  (run-shell-command "firejail firefox"))

(defcommand firefox-p () ()
  "run private firefox window in firejail"
  (run-shell-command "firejail firefox --private-window"))

(defcommand firefox-no-jail () ()
  "run firefox without firejail"
  (run-shell-command "firefox"))

;;; W3M

(defcommand w3m () ()
  (run-raise-or-pull "xterm -class W3M -e w3m duckduckgo.com" '(:class "W3M")))

;;;;;;;;;;;;;;;;;;;;;;;
;;; EMAIL CHAT & IM ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; THUNDERBIRD
(defcommand thunderbird () ()
  (run-raise-or-pull "thunderbird" '((:class "Thunderbird"))))

(defcommand mail () ()
  (run-raise-or-pull "thunderbird" '((:class "Thunderbird"))))
;;; QTOX
(defcommand qtox () ()
  (run-raise-or-pull "qtox" '((:class "qTox"))))
;;; RIOT
(defcommand riot () ()
  (run-raise-or-pull "riot-desktop" '(:class "Riot")))

(defcommand fractal () ()
  (run-raise-or-pull "fractal" '(:class "Fractal")))

(defcommand signal-messenger () ()
  (run-raise-or-pull "signal-desktop" '(:class "Signal")))

;;;;;;;;;;;;;;;;;;;;;
;;; MEDIA PLAYERS ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; VLC MEDIA
(defcommand vlc () ()
  (run-raise-or-pull "vlc" '((:class "vlc"))))

;;; PAROLE MEDIA PLAYER
(defcommand parole-media () ()
  (run-raise-or-pull "parole" '(:class "Parole")))

(defcommand parole-media-new-instance () ()
  (run-shell-command "parole -i"))

;;; MINIMAL VIDEO PLAYER
(defcommand mpv-minimal () ()
  "run mpv with the pseudo-gui frontend"
  (run-raise-or-pull "mpv --player-operation-mode=pseudo-gui" '((:class "mpv"))))

;;; OBS:
(defcommand obs () ()
  (run-raise-or-pull "obs" '(:class "obs")))

;;; ASCII VIDEO PLAYER

;;;;;;;;;;;;;;
;;; SYSTEM ;;;
;;;;;;;;;;;;;;

(defprogram-shortcut thunar)

(defcommand midnight () ()
  (midnight-commander))

(defun midnight-commander ()
  (with-open-window "xterm -e mc" "XTerm"
		    #'(lambda (cwin)
			(setf (window-class cwin) "MC"))))

(defcommand file-manager () ()
  "runs your file manager in the current buffer"
  (run-raise-or-pull "thunar" '(:class "File Manager")))

(defcommand etcher () ()
  (run-shell-command "/opt/Etcher/etcher-electron"))

;;;;;;;;;;;;;;;;;
;;; Documents ;;;
;;;;;;;;;;;;;;;;;

(defcommand libreoffice () ()
  (run-raise-or-pull "libreoffice" '(:class "libreoffice-writer")))

(defcommand emacs () ()
  (run-raise-or-pull "emacs" '((:class "Emacs"))))

;;;;;;;;;;;;
;;; MISC ;;;
;;;;;;;;;;;;

(defcommand deluge () ()
  (run-raise-or-pull "firejail deluge" '(:class "Deluge")))
