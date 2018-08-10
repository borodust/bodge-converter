(cl:in-package :bodge-converter)


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


(defun write-scene (bodge-stream scene-file &key prefix scene-name (embed-textures t))
  (with-bound-scene (scene-file)
    (let ((*node-registry* (make-node-registry)))
      (register-scene-nodes)
      (let* ((scene (ge.rsc:make-empty-scene-resource))
             (*prefix* (or prefix "/"))
             (*images* (list))
             (data (flex:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                     (fill-meshes scene)
                     (fill-materials scene)
                     (fill-animations scene)
                     (fill-nodes scene)
                     (ge.rsc:encode-resource (ge.rsc:make-resource-handler :scene) scene out))))
        (ge.rsc:write-chunk bodge-stream :scene
                            (fad:merge-pathnames-as-file *prefix* (uiop:enough-pathname
                                                                   (or (when scene-name
                                                                         (fad:pathname-as-file scene-name))
                                                                       (file-namestring scene-file))
                                                                   "/"))
                            data)
        (when embed-textures
          (loop for (name relative-path) in *images*
                do (write-image bodge-stream (fad:merge-pathnames-as-file
                                              (fad:pathname-directory-pathname scene-file)
                                              relative-path)
                                :prefix *prefix*
                                :image-name (namestring name)
                                :type :png)))
        scene))))
