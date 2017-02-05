(in-package :cl-user)

(defpackage :bodge-converter.def
  (:use :cl :asdf))

(in-package :bodge-converter.def)


(defsystem bodge-converter
  :description "bodge resource conversion tool"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria log4cl cl-fad classimp rtg-math sdf flexi-streams opticl salza2)
  :serial t
  :components ((:file "packages")
               (:file "brf")
               (:file "image")
               (:file "sdf")
               (:file "assimp")))
