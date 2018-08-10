(cl:in-package :bodge-converter)


(define-constant +max-tex-coord-channels+ 8)
(define-constant +max-color-channels+ 8)
(define-constant +max-bones-per-vertex+ 4)


(declaim (special *scene*
                  *id*
                  *images*
                  *prefix*))


(defparameter *property-class-table* (make-hash-table :test #'equal))


(defun register-material-property-class (name class)
  (setf (gethash name *property-class-table*) class))


(defun find-material-property-class (name)
  (gethash name *property-class-table*))


(defun ai-real (value)
  (float value 0f0))


(defun ai-string-to-lisp (ai-string)
  (claw:c-val ((ai-string (:struct (%ai:string))))
    (cffi:foreign-string-to-lisp (ai-string :data &) :count (ai-string :length))))


(defun ai->mat4 (ai-mat4)
  (claw:c-let ((mat (:struct (%ai:matrix4x4)) :from ai-mat4))
    (ge.ng:mat4 (mat :a1) (mat :a2) (mat :a3) (mat :a4)
                (mat :b1) (mat :b2) (mat :b3) (mat :b4)
                (mat :c1) (mat :c2) (mat :c3) (mat :c4)
                (mat :d1) (mat :d2) (mat :d3) (mat :d4))))


(defun unixify (namestring)
  (substitute #\/ #\\ namestring))


(defmacro with-id-counter (() &body body)
  `(let ((*id* 0))
     ,@body))


(defun next-id ()
  (prog1 *id*
    (incf *id*)))


(defmacro with-ai-struct ((var type value) &body body)
  `(claw:c-let ((,var (:struct (,type)) :from ,(or value var)))
     ,@body))


(defmacro with-scene ((scene-var) &body body)
  `(with-ai-struct (,scene-var %ai:scene *scene*)
     ,@body))


(defmacro with-mesh ((mesh-var &optional mesh-val) &body body)
  `(with-ai-struct (,mesh-var %ai:mesh ,mesh-val)
     ,@body))


(defmacro with-material ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:material ,value)
     ,@body))


(defmacro with-material-property ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:material-property ,value)
     ,@body))


(defmacro with-animation ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:animation ,value)
     ,@body))


(defmacro with-animation-node ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:node-anim ,value)
     ,@body))


(defmacro with-vector-key ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:vector-key ,value)
     ,@body))


(defmacro with-quat-key ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:quat-key ,value)
     ,@body))


(defmacro with-node ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:node ,value)
     ,@body))


(defmacro with-metadata ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:metadata ,value)
     ,@body))


(defmacro with-metadata-entry ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:metadata-entry ,value)
     ,@body))


(defmacro with-bone ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:bone ,value)
     ,@body))


(defmacro with-vertex-weight ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:vertex-weight ,value)
     ,@body))
