(in-package :stumpwm)

(ql:quickload :slynk)

(let ((server-running nil)
      (server-port 4009)
      (old-port nil))
  (defcommand toggle-slynk-server () ()
	      (if server-running
		  (progn
		    (slynk:stop-server old-port)
		    (setf server-running nil)
		    (message "Slynk server stopped on port ~a" old-port))
		  (progn
		    (handler-case
			(slynk:create-server :port server-port
					     :style slynk:*communication-style*
					     :dont-close t)
		      (sb-bsd-sockets:address-in-use-error ()
			(setf server-running t)))
		    (setf old-port server-port)
		    (setf server-running t)
		    (message "Slynk server started on port ~a" server-port))))
  (defun set-slynk-server (port)
    (setf server-port port)))
