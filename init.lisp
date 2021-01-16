(in-package :stumpwm)

(setf *maximum-completions* 50) ; make sure completions dont run off screen

(set-prefix-key (kbd "C-j"))
;; (set-prefix-key (kbd "F20"))

(setf *mouse-focus-policy* :click)

(setf *window-border-style*  :thick
      *normal-border-width*  1
      *maxsize-border-width* 1)

(init-load-path "~/.stumpwm.d/stumpwm-contrib/")

;; Float vlc windows that arent the main window. (eg the open file dialog)
(defun float-vlc-stuff (window)
  (and (not (roled-p window "vlc-main"))
       (classed-p window "vlc")
       (typed-p window :dialog)))

(define-frame-preference nil
    (:float t nil :match-properties-or-function float-vlc-stuff))

;; keep a list of loaded modules. this could be simplified. 
(let ((loaded-modules-list nil))
  (defun use-module (module-name)
    (labels ((%use-module (named-module)
	       (let* ((mod (cond ((stringp named-module) named-module)
				 ((symbolp named-module)
				  (format nil "~a" named-module))
				 (t (error "could not convert datum ~a to an acceptable module name" named-module))))
		      (module (find-module (string-downcase mod))))
		 (if module
		     (progn
		       (asdf:operate 'asdf:load-op module)
		       (unless (member module loaded-modules-list :test #'string=)
			 (setf loaded-modules-list
			       (cons module loaded-modules-list))))
		     (error "Could not load or find module: ~s" module-name)))))
      (if (listp module-name)
	  (mapcar #'%use-module module-name)
	  (%use-module module-name))))
  (defun list-loaded-modules ()
    loaded-modules-list))

(use-module '("hostname" "cpu" "stumptray"))
(stumptray::add-mode-line-hooks)

(setf *window-format* "%m%n%s%20c")

(load "~/.stumpwm.d/utilities.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/rebind-keys.lisp")
(load "~/.stumpwm.d/swank-server.lisp")
(load "~/.stumpwm.d/mode-line.lisp")

;; grabbed pointer anyone?
(setf *grab-pointer-character* 50)
(setf *grab-pointer-character-mask* 51)
;; cool cursors: 40, 32, 30, 48
