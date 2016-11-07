(in-package :bodge-converter)


(declaim (special *scene*
                  *meshes*))


(defun %2a->l (array)
  (loop for v across array collecting
       (concatenate 'list v)))

(defun %a->l (array)
  (concatenate 'list array))


(defun %transpose-flat (array)
  (assert (= (length array) 16))
  (let* ((size 4)
         (r (make-array 16)))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref r (+ (* i size) j))
              (aref array (+ (* j size) i)))))
    r))


(defun %flatten-array (array)
  (loop for v across array appending
       (concatenate 'list v)))


(defun %for-each-child (node fn)
  (loop for child across (ai:children node) do
       (funcall fn child)))


(defun %flat-traverse (root fn)
  (append (funcall fn root)
          (loop for child across (ai:children root) append
               (%flat-traverse child fn))))

(defun %traverse (root fn)
  (append (funcall fn root)
          (loop for child across (ai:children root) collect
               (%traverse child fn))))


(defun %for-each-mesh (node fn)
  (let ((mesh-idxs (ai:meshes node)))
    (loop for idx across mesh-idxs
       for mesh = (aref *meshes* idx)
       collect (funcall fn node mesh idx))))


(defun convert-meshes (out)
  (labels ((%extract-mesh (node mesh idx)
             (list (format nil "~a~a" (ai:name node) idx)
                   :arrays `((0 ,(%2a->l (ai:vertices mesh)))
                             (1 ,(%2a->l (ai:normals mesh))))
                   :face :triangles
                   :indexes (%flatten-array (ai:faces mesh))))
           (%meshes (node)
             (%for-each-mesh node #'%extract-mesh)))
    (loop for mesh in (%flat-traverse (ai:root-node *scene*) #'%meshes) do
         (print '(:mesh) out)
         (pprint mesh out))))


(defun convert-bones (out)
  (declare (special *parent-bone*))
  (let ((bone-table (make-hash-table :test 'equal)))
    (labels ((%extract-bones (node mesh idx)
               (let ((bones (ai:bones mesh)))
                 (when bones
                   (loop for bone across bones
                      for name = (ai:name bone)
                      for registered-bone = (gethash name bone-table)
                      when (null registered-bone) do
                        (setf (gethash name bone-table)
                              (ai:offset-matrix bone))))))
             (%bones (node)
               (%for-each-mesh node #'%extract-bones))

             (%bone-hierarchy (node)
               (let ((name (ai:name node)))
                 (if-let ((offset (gethash name bone-table)))
                   (let ((bone (list (list name
                                           :transform (%a->l (%transpose-flat offset))))))
                     (nconc *parent-bone* (list bone))
                     (let ((*parent-bone* bone))
                       (%for-each-child node #'%bone-hierarchy)))
                   (%for-each-child node #'%bone-hierarchy)))))

      (%traverse (ai:root-node *scene*) #'%bones)
      (let ((*parent-bone* (list nil)))
        (%bone-hierarchy (ai:root-node *scene*))
        (loop for root in (rest *parent-bone*)
           for skeleton-name = (format nil "~a-skeleton" (caar root)) do
             (print '(:skeleton) out)
             (pprint (list skeleton-name root) out))))))


(defmacro with-scene ((path) &body body)
  `(ai:with-log-to-stdout ()
    (let* ((*scene*
            (ai:import-into-lisp
             (cffi-sys:native-namestring (truename ,path))
             :processing-flags '(:ai-process-preset-target-realtime-quality)))
           (*meshes* (ai:meshes *scene*)))
      ,@body)))


(defun convert-to-bodge (in out)
  (with-scene (in)
    (alexandria:with-output-to-file (out out :if-exists :supersede)
      (print '(:brf 1) out)
      (convert-meshes out)
      (convert-bones out))))


(defun print-hierarchy (in)
  (with-scene (in)
    (%traverse (ai:root-node *scene*) (lambda (node)
                                        (list (ai:name node))))))
