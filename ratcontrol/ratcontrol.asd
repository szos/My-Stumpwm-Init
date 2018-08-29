;;;; ratcontrol.asd

(asdf:defsystem #:ratcontrol
  :serial t
  :description "Describe ratcontrol here"
  :author "ratmaster <your.name@example.com>"
  :license "GPLv2"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "ratcontrol")))

