(asdf:defsystem bodge-converter
  :description "cl-bodge resource conversion tool"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria log4cl cl-fad sdf flexi-streams opticl salza2
                          static-vectors
                          cl-bodge/utils cl-bodge/engine
                          assimp-blob bodge-assimp)
  :serial t
  :components ((:file "packages")
               (:file "brf")
               (:file "image")
               (:file "sdf")
               (:file "scene")))
