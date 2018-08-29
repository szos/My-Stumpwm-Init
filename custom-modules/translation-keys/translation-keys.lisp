;;;; translation-keys.lisp

(in-package #:translation-keys)

;;; "translation-keys" goes here. Hacks and glory await!

(export '(define-key-translations translation-keys-help))

(defparameter *keysets*
  (make-hash-table :test #'equal))

(defparameter *help* "")

(setf (gethash :last-binding *keysets*) nil)

(defmacro define-key-translations (class kmap)
  `(progn
     (remhash ,class *keysets*)
     (add-keyset ,class
		 (cons '("M-h" "translation-keys-help"
			 ("Bring up a menu of all bindings"))
		       ',kmap))
     (unless (member 'hangar stumpwm:*focus-window-hook*)
       (stumpwm:add-hook stumpwm:*focus-window-hook* 'hangar))))

(defun add-keyset (class-name kmap)
  (unless (gethash class-name *keysets*)
    (setf (gethash class-name *keysets*) kmap)))

(defun menu-select-cadr (menu-list)
  "takes in a menu just like select-from-menu but only returns the second item"
  (cadr (stumpwm:select-from-menu (stumpwm:current-screen) menu-list)))

(stumpwm:defcommand translation-keys-help () ()
  (stumpwm:message
   (menu-select-cadr 
    `(("Active Key Simulations" ,*help*)
      ("Simulated Applications" 
       ,(let ((hash-printer "Applications with Simulated Keys:~%"))
	  (maphash #'(lambda (key value)
                       (declare (ignore value))
		       (setf hash-printer (concatenate 'string hash-printer (format nil "=> ~S~%" key))))
		   *keysets*)
	  hash-printer))))))

(defun hangar (cwin lwin)
  (declare (ignore lwin))
  (let ((kmap-to-bind (gethash (stumpwm:window-class cwin) *keysets*)))
    (unless (equalp (gethash :last-binding *keysets*) (stumpwm:window-class cwin))
      (when (gethash :last-binding *keysets*)
	(unbind-keyset (gethash (gethash :last-binding *keysets*) *keysets*))
	(setf (gethash :last-binding *keysets*) nil))
      (when kmap-to-bind
	(setf *help* (bind-keyset kmap-to-bind))
	(setf (gethash :last-binding *keysets*) (stumpwm:window-class cwin))))))

(defun bind-keyset (kmap)
  (if kmap
      (destructuring-bind ((binding command &optional (docs)) &rest others) kmap
	(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd binding) command)
	(let ((message-string (concatenate 'string binding " ==> " command "~%"
					   (if docs
					       (concatenate 'string "  " (car docs) "~%~%")
					       "~%")
					   (bind-keyset others))))
	  message-string))
      "~%"))

(defun unbind-keyset (kmap)
  (if kmap
      (destructuring-bind ((binding command &optional (docs)) &rest others) kmap
	(declare (ignore docs))
	(stumpwm:undefine-key stumpwm:*top-map* (stumpwm:kbd binding))
	(let ((message-string (concatenate 'string "Unbound:  " binding " ==> " command "~%"
					   (unbind-keyset others))))
	  message-string))
      "~%"))


;; (translation-keys:define-key-translations "Firefox"
;;     (("C-g" "meta ESC")
;;      ("C-v" "meta SunPageDown")
;;      ("M-v" "meta SunPageUp")
;;      ("C-y" "meta C-v")
;;      ("M-w" "meta C-c")
;;      ("C-w" "meta C-x")
;;      ("C-s" "meta C-f")
;;      ("C-r" "meta C-S-g")
;;      ("C-n" "meta Down")
;;      ("C-p" "meta Up")
;;      ("C-f" "meta Right")
;;      ("C-b" "meta Left")
;;      ("C-B" "meta C-[")
;;      ("C-F" "meta C-]")
;;      ("M-f" "meta C-t")
;;      ("M-b" "meta C-S-t")
;;      ("M-<" "meta Home")
;;      ("M->" "meta End")
;;      ("M-s" "meta C-l")
;;      ("s-f" "meta '")))
