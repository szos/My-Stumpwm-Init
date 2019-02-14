(in-package :stumpwm)
(ql:quickload :xembed)
(ql:quickload :stumpbuffer)
(ql:quickload :clx-truetype)
(ql:quickload :clx)

(setf *timeout-wait* 5)

(setf *message-window-gravity* :top-left)
(setf *input-window-gravity* :top-left)

(set-prefix-key (kbd "C-;"))


(run-shell-command "setxkbmap no")
(run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-no.modmap")
;; (run-shell-command "xmodmap ~/.stumpwm.d/modmaps/eng-altgrn.modmap")
(run-shell-command "/usr/lib/notification-daemon-1.0/notification-daemon")
(run-shell-command "/usr/bin/lxqt-policykit-agent")

(init-load-path "~/.stumpwm.d/custom-modules")

(load-module "stumptray")
(stumptray::add-mode-line-hooks)

(load-module "end-session")

(load-module "net")
;; (load-module "disk")
(load-module "mem")
(load-module "cpu")
(load-module "hostname")
(load-module "battery-portable")

(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Bitstream Vera Sans Mono" :subfamily "Roman" :size 11))

(setf *mode-line-timeout* 1)

(set-fg-color "#000000")
(set-border-color "#000000")
(set-bg-color "#cc3399")

(setf *screen-mode-line-format*
      (list "^6^B%B^b | "
	    ;; theres a problem with the formatters for the cpu module,
	    ;; so were calling the functions explicitly. 
	    '(:eval (let ((usage (cpu::fmt-cpu-usage)) ;; %c
			  (temp (cpu::fmt-cpu-temp))   ;; %t
			  (freq (cpu::fmt-cpu-freq)))  ;; %f
		      (format nil "~A~A ~A" usage temp freq)))
	    " | %l| %M | "
	    "%d
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
(load "~/.stumpwm.d/utilities.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/with-open-window.lisp")
(load "~/.stumpwm.d/fuzzy-finder.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/applications.lisp")
(load "~/.stumpwm.d/sibling.lisp")

(brightness-set 50)

(require :swank)
(swank-loader:init)
(handler-case (swank:create-server :port 4006
				   :style swank:*communication-style*
				   :dont-close t)
  (sb-bsd-sockets:address-in-use-error () (message "Swank Already Running")))

(defun renumber-windows (arg)
  "this needs to be a function which takes an arg. I just throw out the arg.
 --experiment: message the argument!"
  (stumpwm:repack-window-numbers)
  (let ((title (window-title arg)))
    (message "Closed: ~S" (if (> (length title) 30)
			      (subseq title 0 30)
			      title))))

(add-hook *destroy-window-hook* 'renumber-windows)


