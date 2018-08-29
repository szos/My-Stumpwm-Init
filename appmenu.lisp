;; this file is for the appmenu. it contains the appmenu variable as well as
;; the defcommands to run-or-raise specific applications.

("ALL"
 ("NETWORKS-INTERNET"
  ;;submenu
  ("BLUETOOTH"
   ("Bluetooth Manager" bluetooth)
   ("Set Status" "blueman-adapters"))
  ("Firefox" firefox) ;call stumpwm command defined above
  ("Pidgin" "pidgin") ;call terminal application
  ("Mail" "thunderbird")
  ("Deluge Torrent" "deluge-gtk")
  ("Network Connections" "network-status")
  ("Restart Private Internet Access" restart-pia)
  )
 
 ("SYSTEM" 
  ("UTILITY"
   ("Calculator" calculator)
   ("KeePass" keepass)
   ("GParted" gparted))
  ("Terminal" terminal) ; call the terminal application
  ("Retro Term" term)
  ("File Manager" "thunar")
  ("Thunar Settings")
  ("Task Manager" "xfce4-taskmanager")
  ("Temperature sensors" "xfce4-sensors")
  ("Android File Transfer (MTP)" "android-file-transfer")
  ("Cheese Webcam" "cheese")
  ("Menu Editor" MENU)
  )

 ("MEDIA"
  ("VLC" "vlc")
  ("KdenLive" "kdenlive"))

 ("WORK"
  ("Office Suite" "libreoffice")
  ("Mousepad" mousepad)
  )

 ("XFCE"
  ("Logout" log-out)
  ("Screenshot" screenshot)
  ("Task Manager" task-manager)
  ("Terminal" xfce4-terminal)
  ("Power Manager" power-manager)
  ("Settings Manager" xfce4-settings)
  ("Mouse & Touchpad Settings" "xfce4-mouse-settings")
  ("Appfinder" appfinder)))

;; create a project that brings up a tiny terminal (call it tiny term, or TT) from the stumpwm 
;; prompt (C-; ;). it should behave as follows:   run the command to open it from stumpwm. it brings
;; up a small window up at the top of the screen. it lets you run any shell command you want. 
;; its basically a wrapper for exec. 

;; (("INTERNET"
;;   ;;submenu
;;   ("Firefox" firefox) ;call stumpwm command defined above
;;   ("Pidgin" "pidgin") ;call terminal application
;;   ("Thunderbird" "thunderbird")
;;   )
;;  ("SYSTEM" 
;;   ("Terminal" "xfce4-terminal") ; call the terminal application
;;   ("File Manager" "thunar")
;;   )
;;  ("WORK"
;;   ("OpenOffice.org" "openoffice")))


;; (defparameter *app-menu* '(("INTERNET"
;; 	 ;;submenu
;; 	 ("Firefox" firefox) ;call stumpwm command defined above
;; 	 ("Pidgin" "pidgin") ;call terminal application
;; 	 )
;; 	("SYSTEM" 
;; 	 ("Terminal" "xfce4-terminal") ; call the terminal application
;; 	 )
;; 	("WORK"
;; 	 ("OpenOffice.org" "openoffice"))))
