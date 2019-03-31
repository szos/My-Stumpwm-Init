(in-package :stumpwm)
(ql:quickload :xembed)
(ql:quickload :stumpbuffer)
(ql:quickload :clx-truetype)
(ql:quickload :clx)
(ql:quickload :cl-diskspace)

;; (defparameter *window-format-length* 20)

(setf *timeout-wait* 5)

(setf *message-window-gravity* :top-left)
(setf *input-window-gravity* :top-left)
(setf *mouse-focus-policy* :click)

;; (set-prefix-key (kbd "C-;"))
(set-prefix-key (kbd "s-SPC"))

(run-shell-command "setxkbmap no")
(run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-no-super.modmap")
(run-shell-command "/usr/lib/notification-daemon-1.0/notification-daemon")
(run-shell-command "/usr/bin/lxqt-policykit-agent")

(init-load-path "~/.stumpwm.d/custom-modules")

(load-module "stumptray")
(stumptray::add-mode-line-hooks)

(load-module "end-session")

(load-module "net")
(load-module "disk")
(load-module "mem")
(load-module "cpu")
(load-module "hostname")
(load-module "battery-portable")

;; (load-module "ttf-fonts")
;; (set-font (make-instance 'xft:font :family "Bitstream Vera Sans Mono" :subfamily "Roman" :size 11))
;; (set-font "-windows-dina-medium-r-normal--13-100-96-96-c-80-iso8859-1")
(set-font "-xos4-terminus-medium-r-normal--17-120-100-100-c-0-iso10646-1")

(setf *mode-line-timeout* 1)

(set-fg-color "#000000")
(set-border-color "#000000")
(set-bg-color "#cc3399")

(defun get-ip-from-proton ()
  (let ((ip (string-trim '(#\Newline)
			 (run-shell-command "protonvpn-cli --ip" t))))
    (setf *current-ip* ip)))

(defparameter *current-ip* nil)
(defparameter *ip-timer* (run-with-timer 0 120 #'get-ip-from-proton))

(defun format-diskspace (list-of-disks)
  (mapcar (lambda (disk)
	    (let ((disk (getf disk :disk))
		  ;;(percent (getf disk :use-percent))
		  (avail (getf disk :available))
		  (total (getf disk :total)))
	      (format nil "Disk: \"~A\", ~A free of ~A " disk avail total)))
	  list-of-disks))

(defparameter *disk-space* (format-diskspace (cl-diskspace::list-all-disk-info t)))

(run-with-timer 0 1000 (lambda ()
			(setf *disk-space*
			      (format-diskspace (cl-diskspace::list-all-disk-info t)))))
(defun date-format ()
  (let* ((full-date (run-shell-command "date" t))
	 (day ()))
    ))

(setf *screen-mode-line-format*
      (list "^6^B%B^b | "
	    ;; theres a problem with the formatters for the cpu module,
	    ;; so were calling the functions explicitly. 
	    '(:eval (let ((usage (cpu::fmt-cpu-usage)) ;; %c
			  (temp (cpu::fmt-cpu-temp))   ;; %t
			  (freq (cpu::fmt-cpu-freq)))  ;; %f
		      (format nil "~A~A ~A" usage freq temp)))
	    " | %lIP: "
	    '(:eval *current-ip*)
	    " | %M | "
	    "^2^B%d^6^b"
	    ;;'(:eval *disk-space*)
	    
	    "
"
	    "%h | %g | %W"))

(setf *window-format* "%n%s%c")
(set-unfocus-color (first *colors*))
(set-focus-color (cadr *colors*))
(set-win-bg-color (car *colors*))
(setf *normal-border-width* 3)
(setf *window-border-style* :thick)

(when *initializing* 
  (grename "Home")
  (gnewbg "Dev")
  (gnewbg-float "Floater")
  (run-shell-command "nm-applet")
  ;; (run-shell-command "./pia.sh")
  (run-shell-command "blueman-applet"))

(load "~/.stumpwm.d/scheme-macros.lisp")
(load "~/.stumpwm.d/macros.lisp")
(load "~/.stumpwm.d/utilities.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/with-open-window.lisp")
(load "~/.stumpwm.d/fuzzy-finder.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/applications.lisp")
(load "~/.stumpwm.d/sibling.lisp")

(brightness-set 50)

(defun renumber-windows (arg)
  "this needs to be a function which takes an arg. I just throw out the arg.
 --experiment: message the argument!"
  (stumpwm:repack-window-numbers)
  (let ((title (window-title arg)))
    (message "Closed: ~S" (if (> (length title) 30)
			      (subseq title 0 30)
			      title))))

(add-hook *destroy-window-hook* 'renumber-windows)

(require :swank)
(swank-loader:init)
(handler-case (swank:create-server :port 4006
				   :style swank:*communication-style*
				   :dont-close t)
  (sb-bsd-sockets:address-in-use-error () (message "Swank Already Running")))
