(cl:in-package :bodge-converter)


(declaim (special *scene*
                  *id*))

(defun ai-real (value)
  (float value 0f0))


(defmacro with-id-counter (() &body body)
  `(let ((*id* 0))
     ,@body))


(defun next-id ()
  (prog1 *id*
    (incf *id*)))


(defmacro with-scene ((scene-var) &body body)
  `(claw:c-let ((,scene-var (:struct (%ai:scene)) :from *scene*))
     ,@body))


(defmacro with-mesh ((mesh-var &optional mesh-val) &body body)
  `(claw:c-let ((,mesh-var (:struct (%ai:mesh)) :from ,(or mesh-val mesh-var)))
     ,@body))


(defun ai-string-to-lisp (ai-string)
  (claw:c-val ((ai-string (:struct (%ai:string))))
    (cffi:foreign-string-to-lisp (ai-string :data &) :count (ai-string :length))))


;;;
;;; MESHES
;;;

(defun make-attribute-array (source-ptr foreign-type vertex-count element-size)
  (let* ((length (* vertex-count element-size))
         (type (ecase foreign-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))))
         (array (make-static-vector length :element-type type))
         (dst-ptr (static-vector-pointer array)))
    (claw:memcpy dst-ptr source-ptr :n length :type foreign-type)
    array))


(defun set-position-array (bodge-mesh mesh)
  (with-mesh (mesh)
    (let ((vertex-count (mesh :num-vertices)))
      (when (> vertex-count 0)
        (let ((array (make-attribute-array (mesh :vertices) :float vertex-count 3)))
          (setf (ge.rsc:mesh-position-array bodge-mesh) array))))))


(defun copy-indices (array face face-idx primitive-vertex-count)
  (claw:c-val ((face (:struct (%ai:face))))
    (unless (= primitive-vertex-count (face :num-indices))
      (error "Unexpected number of face vertices: expected ~A, but got ~A"
             primitive-vertex-count (face :num-indices)))
    (claw:c-let ((indices :unsigned-int :ptr (face :indices)))
      (loop with start-idx = (* face-idx primitive-vertex-count)
            for i from 0 below primitive-vertex-count
            do (setf (aref array (+ start-idx i)) (indices i))))))


(defun set-index-array (bodge-mesh mesh)
  ;; Single face type assumed
  (with-mesh (mesh)
    (let* ((primitive-vertex-count (eswitch ((mesh :primitive-types) :test #'=)
                                     (%ai:+primitive-type-point+ 1)
                                     (%ai:+primitive-type-line+ 2)
                                     (%ai:+primitive-type-triangle+ 3)))
           (face-count (mesh :num-faces))
           (length (* primitive-vertex-count face-count))
           (array (make-static-vector length :element-type '(unsigned-byte 32))))
      (loop for face-idx below face-count
            do (copy-indices array (mesh :faces * face-idx) face-idx primitive-vertex-count))
      (setf (ge.rsc:mesh-index-array bodge-mesh) array))))


(defun set-normal-array (bodge-mesh mesh)
  (with-mesh (mesh)
    (let ((normal-count (mesh :num-vertices)))
      (when (> normal-count 0)
        (let ((array (make-attribute-array (mesh :normals) :float normal-count 3)))
          (setf (ge.rsc:mesh-normal-array bodge-mesh) array))))))


(defun make-mesh (mesh)
  (with-mesh (mesh)
    (let* ((primitive-type (switch ((mesh :primitive-types) :test #'=)
                            (%ai:+primitive-type-point+ :points)
                            (%ai:+primitive-type-line+ :lines)
                            (%ai:+primitive-type-triangle+ :triangles)
                            (t (error "Unexpected primitive type: ~A" (mesh :primitive-types)))))
           (bodge-mesh (ge.rsc:make-mesh primitive-type)))
      (set-position-array bodge-mesh mesh)
      (set-index-array bodge-mesh mesh)
      (set-normal-array bodge-mesh mesh)
      bodge-mesh)))


(defun fill-meshes (bodge-scene)
  (with-scene (scene)
    (with-mesh (mesh-array (scene :meshes *))
      (loop for mesh-idx below (scene :num-meshes)
            do (setf (ge.rsc:scene-mesh bodge-scene mesh-idx) (make-mesh (mesh-array mesh-idx)))))))


;;;
;;; SCENE
;;;
(claw:defcallback write-log :void ((message :pointer) (data :pointer))
  (declare (ignore data))
  (format *standard-output* "~A" (cffi:foreign-string-to-lisp message)))


(defmacro with-logging (() &body body)
  (with-gensyms (logger)
    `(claw:c-with ((,logger (:struct (%ai:log-stream))))
       (setf (,logger :callback) (claw:callback 'write-log))
       (%ai:attach-log-stream ,logger)
       (unwind-protect
            (progn ,@body)
         (%ai:detach-log-stream ,logger)))))


(defmacro with-bound-scene ((path) &body body)
  (once-only (path)
    `(with-logging ()
       (let* ((*scene* (%ai:import-file (namestring (truename ,path))
                                        (logior %ai:+process-preset-target-realtime-max-quality+
                                                %ai:+process-optimize-graph+
                                                %ai:+process-debone+
                                                %ai:+process-sort-by-p-type+
                                                %ai:+process-triangulate+))))
         (unless *scene*
           (error "Failed to parse asset file '~A'" ,path))
         (unwind-protect
              (progn ,@body)
           (%ai:release-import *scene*))))))


(defun write-scene (bodge-stream scene-name scene-file &key prefix)
  (with-bound-scene (scene-file)
    (let* ((scene (ge.rsc:make-empty-scene))
           (data (flex:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                   (fill-meshes scene)
                   (ge.rsc:encode-resource (ge.rsc:make-resource-handler :scene) scene out))))
      (ge.rsc:write-chunk bodge-stream :scene
                          (or prefix (format nil "/~A" scene-name))
                          data)
      (values))))
