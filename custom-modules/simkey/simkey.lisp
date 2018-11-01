;;;; simkey.lisp

(in-package #:simkey)

;;; "simkey" goes here. Hacks and glory await!

(defparameter *translation-commands* (make-hash-table :test 'equalp))

(defparameter *top-binds* nil)

(defmacro define-top-map-key (key command)
  "this works best! it stores things in actual code which will
let us call it in the definer macro. "
  `(progn
     (stumpwm::define-key stumpwm::*top-map* ,key ,command)
     (if (assoc ',key *top-binds* :test #'equal)
	 (setf (cdr (assoc ',key *top-binds* :test #'equal)) ,command)
	 (push '(,key . ,command) *top-binds*))))

(defun remap-top-binds ()
  (mapcar #'(lambda (element)
	      `(,(car element) ,(cdr element)))
	  *top-binds*))

(defmacro define-key-translation (class &body kmap)
  "this takes a class and a keymap (same as interactive-keymap) and defines an
interactive keymap with no return bindings, and adds the key to our database of
key translations"
  `(let ((kmap-name ,(format nil "~A-translations" class)))
     (stumpwm::define-interactive-keymap-no-return ,(intern (format nil "~A-translations" class)) ()
       ,@(remap-top-binds)
       ,@kmap)
     (add-keys-to-hash ,class ,(format nil "~A-translations" class))
     kmap-name))

(defun trans-keys-hangar (cwin lwin)
  "this hangs on the focus window hook and manages translations. 
if theres a previous translation active, we exit it. otherwise we
if the current window has a translation we set the previous translation to
it and then enter that translation map. "
  (declare (ignore lwin))
  (let ((cwin-hash-cmd (gethash (stumpwm::window-class cwin) *translation-commands*)))
    (unbind-previous-map)
    (when cwin-hash-cmd
      (bind-previous-map cwin-hash-cmd)
      (stumpwm::run-commands
       (format nil "~A" cwin-hash-cmd)))))

(defun unbind-previous-map ()
  "checks if there is a previous map active. if there is,
we run the exit command. otherwise we do nothing. "
  (let ((prev-map (gethash :previous *translation-commands*)))
    (when prev-map
      (setf (gethash :previous *translation-commands*) nil)
      (stumpwm::run-commands
       (format nil "EXIT-~A" prev-map)))))

(defun bind-previous-map (map-name)
  "we store the map in the hash table so we know which one to exit. "
  (setf (gethash :previous *translation-commands*) map-name))

(defun add-keys-to-hash (class cmd-name)
  "adds our class to the hash table, with a value of the command name. "
  (setf (gethash class *translation-commands*) cmd-name))
