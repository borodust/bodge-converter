(cl:in-package :bodge-converter)


(declaim (special *scene*
                  *meshes*))


(define-constant +bones-per-vertex+ 4)


(defun prefixify (name prefix)
  (format nil "~A~A" (or prefix "") name))


(defun search-sorted (value sorted-array &key (test #'eql) (predicate #'<) (key #'identity))
  (labels ((%aref (idx)
             (aref sorted-array idx))
           (%compare (idx)
             (funcall predicate value (funcall key (%aref idx))))
           (%test (idx)
             (funcall test value (funcall key (%aref idx))))
           (%search (start end)
             (if (= start end)
                 (values nil end)
               (let ((idx (floor (/ (+ start end) 2))))
                 (if (%test idx)
                     (values (%aref idx) idx)
                     (if (%compare idx)
                         (%search start idx)
                         (%search (1+ idx) end)))))))
    (%search 0 (length sorted-array))))


(defun %2a->l (array)
  (loop for v across array collecting
       (concatenate 'list v)))

(defun %a->l (array)
  (concatenate 'list array))


(defun %a->m4 (arr)
  (macrolet ((assign ()
               `(m4:make ,@(loop for i below 16 collecting
                                `(aref arr ,i)))))
    (assign)))


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


(defun ensure-component-size (list default-value)
  (loop for component in list
     for size = (length component) collect
    (if (> size +bones-per-vertex+)
        (prog1
            (loop repeat +bones-per-vertex+
               for el = component then (cdr el)
               finally
                 (setf (cdr el) nil)
                 (return component))
          (log:warn "~a bones for vertex found: expected ~a components at most"
                    size +bones-per-vertex+))
        (loop for i from 0 below (- +bones-per-vertex+ size)
           collecting default-value into tail
           finally (return (append component tail))))))


(defun weight-and-bones (weights bones count)
  (loop for id from 0 below count
     collect
       (gethash id weights)
     into weight-list
     collect
       (gethash id bones)
     into bone-list
     finally (return (list `(2 ,(ensure-component-size weight-list 0.0))
                           `(3 ,(ensure-component-size bone-list 0))))))


(defun collect-weights (bones vertex-count)
  (unless (null bones)
    (loop with weights = (make-hash-table) and
       bone-ids = (make-hash-table)
       for bone-idx being the hash-keys in bones using (hash-value bone) do
         (loop for vertex-weight across (ai:weights bone) do
              (let ((id (ai:id vertex-weight))
                    (weight (ai:weight vertex-weight)))
                (push weight (gethash id weights))
                (push bone-idx (gethash id bone-ids))))
       finally (return (weight-and-bones weights bone-ids vertex-count)))))


(defun convert-meshes (out prefix)
  (labels ((%appendable (idx 2d-array)
             (unless (null 2d-array)
               (list `(,idx ,(%2a->l 2d-array)))))
           (%bake-transform (node)
             (if (null node)
                 (m4:identity)
                 (m4:* (%bake-transform (ai:parent node)) (%a->m4 (ai:transform node)))))
           (%extract-mesh (node mesh idx)
             (declare (ignore idx))
             (let* ((name (format nil "mesh.~A" (ai:name node)))
                    (bones (unless (null (ai:bones mesh))
                             (loop with r = (make-hash-table :test 'eql)
                                for bone across (ai:bones mesh)
                                for i = 1 then (1+ i) do
                                  (setf (gethash i r) bone)
                                finally (return r))))
                    (vertices (ai:vertices mesh))
                    (arrays (append
                             (list `(0 ,(%2a->l vertices)))
                             (%appendable 1 (ai:normals mesh))
                             (collect-weights bones (length vertices))
                             (when-let ((tex-coords (ai:texture-coords mesh)))
                               (loop for coords across tex-coords
                                  for idx = 0 then (1+ idx) collect
                                    `(,(+ idx 10) ,(%2a->l coords)))))))
               (list name
                     :transform (%a->l (m4:transpose (%bake-transform node)))
                     :arrays arrays
                     :face :triangles
                     :indexes (%flatten-array (ai:faces mesh))
                     :bones (unless (null bones)
                              (loop for i being the hash-keys in bones
                                 using (hash-value bone) collect
                                   (list (format nil "~a.bone.~a" name i)
                                         :index i
                                         :bone (ai:name bone)
                                         :offset (%a->l (ai:offset-matrix bone))))))))
           (%meshes (node)
             (%for-each-mesh node #'%extract-mesh)))
    (loop for mesh in (%flat-traverse (ai:root-node *scene*) #'%meshes)
       do (let ((data (flex:with-output-to-sequence (stream)
                        (with-character-stream (stream)
                          (prin1 mesh stream)))))
            (with-character-stream (out)
              (prin1 (list :mesh :name (prefixify (first mesh) prefix) :size (length data)) out))
            (write-sequence data out)))))


(declaim (special *parent-bone* *root*))
(defun convert-bones (out prefix)
  (let ((bone-table (make-hash-table :test 'equal)))
    (labels ((%register-parents (node)
               (if (gethash (ai:name node) bone-table)
                   (if (null *root*)
                       (let ((*root* node))
                         (%for-each-child node #'%register-parents))
                       (loop for parent = (ai:parent node) then (ai:parent parent)
                          until (gethash (ai:name parent) bone-table)
                          do (setf (gethash (ai:name parent) bone-table) t)
                          finally (%for-each-child node #'%register-parents)))
                   (%for-each-child node #'%register-parents)))

             (%extract-mesh-bones (node mesh idx)
               (declare (ignore node idx))
               (let ((bones (ai:bones mesh)))
                 (when bones
                   (loop for bone across bones
                      for name = (ai:name bone)
                      for registered-bone = (gethash name bone-table)
                      when (null registered-bone) do
                        (setf (gethash name bone-table) t)))))

             (%extract-animation-bones (animation)
               (loop for chan across (ai:channels animation)
                  do (setf (gethash (ai:node-name chan) bone-table) t)))

             (%bones (node)
               (%for-each-mesh node #'%extract-mesh-bones))

             (%bone-hierarchy (node)
               (let ((name (ai:name node)))
                 (if (gethash name bone-table)
                     (let ((bone (list
                                  (list name
                                        :transform (%a->l (ai:transform node))))))
                       (nconc *parent-bone* (list bone))
                       (let ((*parent-bone* bone))
                         (%for-each-child node #'%bone-hierarchy)))
                     (%for-each-child node #'%bone-hierarchy)))))

      (%traverse (ai:root-node *scene*) #'%bones)
      (when-let ((animations (ai:animations *scene*)))
        (loop for ani across animations
           do (%extract-animation-bones ani)))
      (let ((*root* nil))
        (%register-parents (ai:root-node *scene*)))
      (let ((*parent-bone* (list nil)))
        (%bone-hierarchy (ai:root-node *scene*))
        (loop for root in (rest *parent-bone*)
           do (let ((data (flex:with-output-to-sequence (stream)
                            (with-character-stream (stream)
                              (prin1 root stream)))))
                (with-character-stream (out)
                  (prin1 (list :skeleton :name (prefixify (format nil "skeleton.~A" (caar root)) prefix)
                               :size (length data))
                         out))
                (write-sequence data out)))))))


(defun interpolated-value (timestamp frames type name)
  (labels ((%a->q (a)
             (q:q! (aref a 0) (aref a 1) (aref a 2) (aref a 3)))
           (%a->v (a)
             (v3:make (aref a 0) (aref a 1) (aref a 2)))
           (%q->l (quat)
             (list (q:x quat) (q:y quat) (q:z quat) (q:w quat)))
           (%v->l (v)
             (list (v:x v) (v:y v) (v:z v)))
           (%interpolate (this-idx that-idx)
             (let* ((this (aref frames this-idx))
                    (that (aref frames that-idx))
                    (this-timestamp (ai:key-time this))
                    (f (float (/ (- timestamp this-timestamp)
                                 (- (ai:key-time that) this-timestamp)))))
               (log:debug "Interpolating for '~a' at ~a" name timestamp)
               (ecase type
                 (:quat (%q->l (q:slerp (%a->q (ai:value this)) (%a->q (ai:value that)) f)))
                 (:vec (%v->l (v:lerp (%a->v (ai:value this)) (%a->v (ai:value that)) f)))))))
    (multiple-value-bind (frame idx) (search-sorted timestamp frames :key #'ai:key-time)
      (let* ((len (length frames)))
        (if (null frame)
            (cond
              ((= idx 0) (%a->l (ai:value (aref frames 0))))
              ((= idx len) (%a->l (ai:value (aref frames (1- len)))))
              (t (%interpolate (1- idx) idx)))
            (%a->l (ai:value frame)))))))


(defun convert-keyframes (chan tps)
  (declare (ignore tps))
  (labels ((%sort-by-time (seq)
             (sort seq #'< :key #'ai:key-time)))
    (let* ((name (ai:node-name chan))
           (rotations (%sort-by-time (ai:rotation-keys chan)))
           (positions (%sort-by-time (ai:position-keys chan)))
           (scales (%sort-by-time (ai:scaling-keys chan)))
           (timestamps (remove-duplicates
                        (sort (mapcar #'ai:key-time
                                      (concatenate 'list rotations positions scales))
                              #'<))))
      ;; fixme: apply tps (ticks-per-second) and check .fbx animation import after
      (loop for timestamp in timestamps collect
           (list timestamp
                 (interpolated-value timestamp rotations :quat name)
                 (interpolated-value timestamp positions :vec name)
                 (interpolated-value timestamp scales :vec name))))))


(defun convert-animation (out prefix)
  (when-let ((ani (ai:animations *scene*)))
    (loop for ani across ani
       for idx = 0 then (1+ idx)
       for ani-name = (format nil "animation.~a"  (ai:name ani))
       for tps = (if (= (ai:ticks-per-second ani) 0) 1 (ai:ticks-per-second ani))
       do (let* ((chans (loop for chan across (ai:channels ani)
                           for ch-name = (ai:node-name chan) collect
                             (append (list (list (format nil "~a.~a.sequence" ani-name ch-name)
                                                 :bone ch-name))
                                     (convert-keyframes chan tps))))
                 (data (flex:with-output-to-sequence (stream)
                         (with-character-stream (stream)
                           (prin1 (append (list ani-name) chans) stream)))))
            (with-character-stream (out)
              (prin1 (list :animation :name (prefixify ani-name prefix) :size (length data)) out))
            (write-sequence data out)))))



(defmacro with-scene ((path) &body body)
  `(ai:with-log-to-stdout ()
    (let* ((*scene*
            (ai:import-into-lisp
             (cffi-sys:native-namestring (truename ,path))
             :processing-flags '(:ai-process-preset-target-realtime-quality)))
           (*meshes* (ai:meshes *scene*)))
      ,@body)))


(defun assimp-to-bodge (bodge-stream asset-file &optional (prefix "/"))
  (with-scene (asset-file)
    (convert-meshes bodge-stream prefix)
    (convert-bones bodge-stream prefix)
    (convert-animation bodge-stream prefix)))


(defun print-hierarchy (in)
  (with-scene (in)
    (%traverse (ai:root-node *scene*) (lambda (node)
                                        (list (ai:name node))))))
