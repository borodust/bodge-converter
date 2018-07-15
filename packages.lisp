(cl:defpackage :bodge-converter
  (:use :cl :alexandria :static-vectors)
  (:export with-new-resource-file
           write-scene
           write-image
           write-sdf-font
           dump-sdf-atlas))
