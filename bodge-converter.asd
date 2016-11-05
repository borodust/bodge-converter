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
  :depends-on (alexandria log4cl cl-fad)
  :serial t
  :components ((:file "packages")))
