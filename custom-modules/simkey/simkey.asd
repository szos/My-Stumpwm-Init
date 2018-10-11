;;;; simkey.asd

(asdf:defsystem #:simkey
  :description "Describe simkey here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "simkey")))

