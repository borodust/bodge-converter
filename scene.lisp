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



(defun write-bytes (stream source-ptr byte-length)
  (with-static-vectors ((byte-vec byte-length :element-type '(unsigned-byte 8)))
    (claw:memcpy (static-vector-pointer byte-vec) source-ptr
                 :n byte-length :type :unsigned-char)
    (write-sequence byte-vec stream)))


(defun write-descriptor (stream type &rest params &key &allow-other-keys)
  (with-character-stream (stream)
    (prin1 (nconc (list type) params) stream)))

;;;
;;; MESHES
;;;
(defun write-mesh-positions (out mesh)
  (with-mesh (mesh)
    (let* ((vertex-count (mesh :num-vertices))
           (element-size 3)
           (byte-length (* vertex-count element-size (claw:sizeof :float))))
      (when (> vertex-count 0)
        (write-descriptor out :position-array :count vertex-count
                                              :element-size element-size
                                              :element-type :float)
        (write-bytes out (mesh :vertices) byte-length)))))


(defun write-mesh-indices (out face)
  (claw:c-val ((face (:struct (%ai:face))))
    (write-bytes out (face :indices) (* (face :num-indices)
                                        (claw:sizeof :unsigned-int)))))


(defun write-mesh-faces (out mesh)
  (with-mesh (mesh)
    (let ((primitive-vertex-count (eswitch ((mesh :primitive-types) :test #'=)
                                    (%ai:+primitive-type-point+ 1)
                                    (%ai:+primitive-type-line+ 2)
                                    (%ai:+primitive-type-triangle+ 3)))
          (face-count (mesh :num-faces)))
      (write-descriptor out :index-array :count (* primitive-vertex-count face-count)
                                         :element-type :unsigned-int)
      (loop for face-idx below face-count
            do (write-mesh-indices out (mesh :faces * face-idx))))))


(defun write-mesh-normals (out mesh)
  (with-mesh (mesh)
    (let ((normal-count (mesh :num-vertices))
          (element-size 3))
      (when (> normal-count 0)
        (write-descriptor out :normal-array :count normal-count
                                            :element-size element-size
                                            :element-type :float)
        (write-bytes out (mesh :normals) (* normal-count
                                            element-size
                                            (claw:sizeof :float)))))))


(defun write-mesh-attributes (out mesh)
  (write-mesh-positions out mesh)
  (write-mesh-faces out mesh)
  (write-mesh-normals out mesh))


(defun write-mesh (out mesh mesh-idx)
  (with-mesh (mesh)
    (let ((data (flex:with-output-to-sequence (stream)
                  (write-mesh-attributes stream mesh)))
          (primitive-type (switch ((mesh :primitive-types) :test #'=)
                            (%ai:+primitive-type-point+ :points)
                            (%ai:+primitive-type-line+ :lines)
                            (%ai:+primitive-type-triangle+ :triangles)
                            (t (error "Unexpected primitive type: ~A" (mesh :primitive-types))))))
      (write-descriptor out :mesh :index mesh-idx
                                  :primitive primitive-type
                                  :size (length data))
      (write-sequence data out))))


(defun write-meshes (out)
  (with-scene (scene)
    (with-mesh (mesh-array (scene :meshes *))
      (loop for mesh-idx below (scene :num-meshes)
            do (write-mesh out (mesh-array mesh-idx) mesh-idx)))))


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
    (let* ((data (flex:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                   (write-meshes out))))
      (write-descriptor bodge-stream :scene :name (or prefix (format nil "/~A" scene-name))
                                            :size (length data))
      (write-sequence data bodge-stream)
      (values))))
