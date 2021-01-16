(in-package :stumpwm)

(defmacro define-keymap (keymap-to-exit &body bindings)
  "this macro defines a small local keymap based on the bindings variable. 
This macro also takes a variable keymap-to-exit. This variable adds in 
functionality similar to define-interactive-keymap to exit the map on a keypress.
for example: 
\(define-interactive-keymap example ()
   \(\(kbd \"v\"\) \"run-some-command\"\)
   \(\(kbd \"x\"\) 
     \(define-keymap example 
        \(\(kbd \"h\"\) \"other-command\" t\)\)\)\)
in the above keymap, when one presses x while in the example interactive keymap,
it will activate the keymap defined by define-keymap. when in this keymap if we 
press h it will run the command other-command and then exit the interactive 
keymap example. "
  (let* ((m (gensym))
         (bind-to-m (mapcar
		     (lambda (bind)
		       `(define-key ,m ,(first bind)
			  ,(if (and (third bind) keymap-to-exit)
			       (concatenate 'string "call-and-exit-kmap \""
					    (second bind) "\" "
					    (format nil "EXIT-~A"
						    keymap-to-exit))
			       (second bind))))
		     bindings)))
    `(let ((,m (make-sparse-keymap)))
       ,@bind-to-m
       ,m)))

(define-key *root-map* (kbd "ooblique") "colon")
(define-key *root-map* (kbd "C-ooblique") "colon")
(define-key *root-map* (kbd "r") "remove-split")
(define-key *root-map* (kbd "ae") "windowlist %n %c %t")
(define-key *root-map* (kbd "C-o") "fnext")
(define-key *root-map* (kbd "M-c") "exec st")

(define-key *root-map* (kbd "j")
  (define-keymap nil
    ((kbd "f") "file-manager")
    ((kbd "b") "browser")
    ((kbd "k") "setxkbmap")
    ((kbd "x") "xbacklight")
    ((kbd "v") "vlc")
    ((kbd "j") "toggle-jiggle")))
