;;;; translating-tests.lisp

(in-package #:translating-tests)

;;; "translating-tests" goes here. Hacks and glory await!

(defparameter *translation-commands* (make-hash-table :test 'equalp))

(defparameter *prefix-key* nil)

(defun set-translations-prefix (key)
  (setf *prefix-key* key))

(defmacro def-trans-keys (class &body kmap)
  `(let ((kmap-name ,(format nil "~A-translations" class)))
     (stumpwm::define-interactive-keymap-no-return ,(intern (format nil "~A-translations" class)) ()
       ((stumpwm::kbd "C-;") stumpwm::*root-map*)
       ,@kmap)
     (add-keys-to-hash ,class ,(format nil "~A-translations" class))))

;; (&key (prefix *prefix-key*))



(defun trans-keys-hangar (cwin lwin)
  (let ((cwin-hash-cmd (gethash (stumpwm::window-class cwin) *translation-commands*)))
    (when lwin
      (when (gethash (stumpwm::window-class lwin) *translation-commands*) ;; if the class has an associated command, we call its exit version
	(stumpwm::run-commands
	 (format nil "EXIT-~A" (gethash (stumpwm::window-class lwin) *translation-commands*)))))
    (when cwin-hash-cmd
      (stumpwm::run-commands
       (format nil "~A" cwin-hash-cmd)))))

(defun add-keys-to-hash (class cmd-name)
  (setf (gethash class *translation-commands*) cmd-name))

(defun gen-exit (name)
  (format nil "exit-~A" name))

