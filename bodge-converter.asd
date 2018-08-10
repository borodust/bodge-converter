(asdf:defsystem bodge-converter
  :description "cl-bodge resource conversion tool"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria log4cl cl-fad sdf flexi-streams opticl salza2
                          static-vectors
                          cl-bodge/utils cl-bodge/engine cl-bodge/resources
                          assimp-blob bodge-assimp)
  :serial t
  :components ((:file "packages")
               (:file "brf")
               (:file "image")
               (:file "sdf")
               (:module scene
                :serial t
                :components ((:file "utils")
                             (:file "node")
                             (:file "material")
                             (:file "mesh")
                             (:file "animation")
                             (:file "scene")))))
