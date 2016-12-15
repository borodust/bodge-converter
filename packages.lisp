(in-package :bodge-converter.def)


(defpackage :bodge-converter
  (:use :cl :alexandria)
  (:export print-hierarchy
           assimp-to-bodge
           sdf-to-bodge))
