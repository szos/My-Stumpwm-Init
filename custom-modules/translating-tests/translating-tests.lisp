;;;; translating-tests.lisp

(in-package #:translating-tests)

;;; "translating-tests" goes here. Hacks and glory await!

(defparameter *translation-commands* (make-hash-table :test 'equalp))

(defmacro def-trans-keys (class &body kmap)
  `(let ((kmap-name ,(format nil "~A-translations" class)))
     (stumpwm::define-interactive-keymap-no-return ,(intern (format nil "~A-translations" class)) ()
       ,@kmap)
     (add-keys-to-hash ,class ,(format nil "~A-translations" class))))

(def-trans-keys "Firefox" ((kbd "C-p") "meta SunPageUp"))

(defun trans-keys-hangar (cwin lwin)
  (let ((lwin-hash-cmd (gethash (stumpwm::window-class lwin) *translation-commands*));; returns string
	(cwin-hash-cmd (gethash (stumpwm::window-class cwin) *translation-commands*)))
    (when lwin-hash-cmd ;; if the class has an associated command, we call its exit version
      (stumpwm::run-commands
       (format nil "EXIT-~A" lwin-hash-cmd)))
    (when cwin-hash-cmd
      (stumpwm::run-commands
       (format nil "~A" cwin-hash-cmd)))))

(defun add-keys-to-hash (class cmd-name)
  (unless (gethash class *translation-commands*)
    (setf (gethash class *translation-commands*) cmd-name)))

(defun gen-exit (name)
  (format nil "exit-~A" name))

