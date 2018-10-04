;;;;;;;;;;;;;;;;;;;;
;;; APPLICATIONS ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; WEB BROWSERS ;;;
;;;;;;;;;;;;;;;;;;;;

(defcommand web-browser () ()
  (tor))

;;; TOR
(defcommand tor () ()
  (run-raise-or-pull "./TOR/Browser/start-tor-browser" '(:class "Tor Browser")))

;;; FIREFOX
(defcommand firefox () ()
  "run firefox or set focus to it f already running"
  (run-raise-or-pull "firefox" '(:class "Firefox")))

(defcommand firefox-n () ()
  "run firefox"
  (run-shell-command "firefox"))

;;; WATERFOX
(defcommand waterfox () ()
  "Run or raise or list waterfox"
  (run-raise-or-pull "waterfox" '((:class "Waterfox"))))

(defcommand waterfox-n () ()
  "run waterfox"
  (run-shell-command "waterfox"))

(defcommand waterfox-p () ()
  "run private window"
  (run-shell-command "waterfox --private-window"))

;;; CONKEROR
(defcommand conkeror () ()
  "run conkeror or raise it"
  (run-raise-or-pull "conkeror" '(:class "Conkeror")))

(defcommand conkeror-n () ()
  "run a new conkeror instance"
  (run-shell-command "conkeror"))

;;; W3M
(defcommand w3m () ()
  (run-raise-or-pull
   '(with-open-window "xterm -e w3m duckduckgo.com/lite/" "XTerm"
     #'reclassify-window "W3M")
   '(:class "W3M")))

(defcommand w3m-new () ()
  "runs w3m open to duckduckgo"
  (with-open-window "xterm -e w3m -s duckduckgo.com/lite/" "XTerm"
     #'reclassify-window "W3M"))

(defcommand w3m-manual () ()
  "runs w3m open the manual (cloned locally)"
  (with-open-window "xterm -e w3m  ~/w3m.manual.html" "XTerm"
     #'reclassify-window "W3M Manual"))

;;;;;;;;;;;;;;;;;;;;;;;
;;; EMAIL CHAT & IM ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; THUNDERBIRD
(defprogram-shortcut thunderbird)

(defcommand mail () ()
  (run-raise-or-pull "thunderbird" '((:class "Thunderbird"))))
;;; QTOX
(defcommand qtox () ()
  (run-raise-or-pull "qtox" '((:class "qTox"))))
;;; RIOT
(defcommand riot () ()
  (run-raise-or-pull "riot-desktop" '(:class "Riot")))

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

(defcommand file-manager () ()
  "runs your file manager in the current buffer"
  (run-raise-or-pull "thunar" '(:class "File Manager")))
