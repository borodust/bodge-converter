(in-package :bodge-converter)


(defun collada->bodge (in out)
  (ai:with-log-to-stdout ()
    (let* ((scene
            (ai:import-into-lisp
             (cffi-sys:native-namestring (truename in))
             :processing-flags '(:ai-process-preset-target-realtime-quality)))
           (scene-meshes  (ai:meshes scene)))
      (labels ((%array->list (array)
                 (loop for v across array collecting
                      (concatenate 'list v)))
               (%flatten (array)
                 (loop for v across array appending
                      (concatenate 'list v)))
               (%meshes (node)
                 (let* ((id-prefix (ai:name node))
                        (mesh-idxs (ai:meshes node))
                        (meshes (loop for idx across mesh-idxs
                                   for mesh = (aref scene-meshes idx)
                                   collect
                                     (list (format nil "~a~a" id-prefix idx)
                                           :arrays `((0 ,(%array->list (ai:vertices mesh)))
                                                     (1 ,(%array->list (ai:normals mesh))))
                                           :face :triangles
                                           :indexes (%flatten (ai:faces mesh))))))
                   (append meshes (loop for child across (ai:children node) append
                                       (%meshes child))))))
        (alexandria:with-output-to-file (out out :if-exists :supersede)
          (print '(:brf 1) out)
          (loop for mesh in (%meshes (ai:root-node scene)) do
               (print '(:mesh) out)
               (pprint mesh out)))))))
