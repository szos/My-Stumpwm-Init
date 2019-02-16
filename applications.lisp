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
    (run-raise-or-pull "firejail firefox -new-instance -P I2P -url 127.0.0.1:7657/home"
		       '(:class "Firefox"))))
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

;;; WATERFOX
(defcommand waterfox () ()
  "Run or raise or list waterfox"
  (run-raise-or-pull "firejail waterfox" '((:class "Waterfox"))))

(defcommand waterfox-n () ()
  "run waterfox"
  (run-shell-command "firejail waterfox"))

(defcommand waterfox-p () ()
  "run private window"
  (run-shell-command "firejail waterfox --private-window"))

;;; CONKEROR
(defcommand conkeror () ()
  "run conkeror or raise it"
  (run-raise-or-pull "conkeror" '(:class "Conkeror")))

(defcommand conkeror-n () ()
  "run a new conkeror instance"
  (run-shell-command "firejail conkeror"))

;;; ICECAT
(defcommand icecat () ()
  "run icecat"
  (run-raise-or-pull "~/icecat/icecat" '(:class "Icecat")))

(defcommand icecat-n () ()
  "run new icecat instance"
  (run-shell-command "firejail ~/icecat/icecat"))

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
