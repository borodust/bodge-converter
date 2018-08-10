(cl:in-package :bodge-converter)


;;;
;;; MESHES
;;;
(defun make-attribute-array-from-continuous (source-ptr foreign-type vertex-count element-size)
  (let* ((length (* vertex-count element-size))
         (type (ecase foreign-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))
                 (:int '(signed-byte 32))))
         (array (make-static-vector length :element-type type))
         (dst-ptr (static-vector-pointer array)))
    (claw:memcpy dst-ptr source-ptr :n length :type foreign-type)
    array))


(defun make-attribute-array-from-strided (source-ptr foreign-type vertex-count element-size
                                          skipped-element-count)
  ;; We heard you like C so we added a bit of it into your CL
  (let* ((length (* vertex-count element-size))
         (type (ecase foreign-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))
                 (:int '(signed-byte 32))))
         (array (make-static-vector length :element-type type))
         (dst-ptr (static-vector-pointer array))
         (src-element-byte-size (* (claw:sizeof foreign-type) (+ element-size skipped-element-count)))
         (dst-element-byte-size (* (claw:sizeof foreign-type) element-size))
         (src-addr (cffi:pointer-address source-ptr))
         (dst-addr (cffi:pointer-address dst-ptr)))
    (loop for i below vertex-count
          for from-addr = (+ src-addr (* src-element-byte-size i))
          for to-addr = (+ dst-addr (* dst-element-byte-size i))
          do (claw:memcpy (cffi:make-pointer to-addr)
                          (cffi:make-pointer from-addr)
                          :n element-size
                          :type foreign-type))
    array))


(defun make-attribute-array (source-ptr foreign-type vertex-count element-size
                             &optional (skipped-element-count 0))
  (if (zerop skipped-element-count)
      (make-attribute-array-from-continuous source-ptr foreign-type vertex-count element-size)
      (make-attribute-array-from-strided source-ptr foreign-type vertex-count element-size
                                         skipped-element-count)))


(defun set-position-array (bodge-mesh mesh)
  (with-mesh (mesh)
    (let ((vertex-count (mesh :num-vertices)))
      (when (> vertex-count 0)
        (let ((array (make-attribute-array (mesh :vertices) :float vertex-count 3)))
          (setf (ge.rsc:mesh-resource-position-array bodge-mesh) array))))))


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
      (setf (ge.rsc:mesh-resource-index-array bodge-mesh) array))))


(defun set-normal-array (bodge-mesh mesh)
  (with-mesh (mesh)
    (let ((normal-count (mesh :num-vertices)))
      (when (> normal-count 0)
        (let ((array (make-attribute-array (mesh :normals) :float normal-count 3)))
          (setf (ge.rsc:mesh-resource-normal-array bodge-mesh) array))))))


(defun set-tangent-array (bodge-mesh mesh)
  (with-mesh (mesh)
    (let ((tangent-count (mesh :num-vertices)))
      (when (> tangent-count 0)
        (let ((array (make-attribute-array (mesh :tangents) :float tangent-count 3)))
          (setf (ge.rsc:mesh-resource-tangent-array bodge-mesh) array))))))


(defun set-skeleton (bodge-mesh mesh)
  (with-mesh (mesh)
    (let* ((vertex-count (mesh :num-vertices))
           (vertex-bone-array (make-array vertex-count :initial-element nil))
           (bone-id-array (make-array (* vertex-count +max-bones-per-vertex+)
                                      :element-type '(signed-byte 32)
                                      :initial-element -1))
           (bone-weight-array (make-array (* vertex-count +max-bones-per-vertex+)
                                          :element-type 'single-float
                                          :initial-element 0f0)))
      (loop for bone-idx below (mesh :num-bones)
            do (with-bone (bone (mesh :bones * bone-idx))
                 (let* ((node (find-node-by-name (ai-string-to-lisp (bone :name))))
                        (bodge-bone (ge.rsc:make-mesh-resource-bone (node-id node)
                                                                    (ai->mat4 (bone :offset-matrix)))))
                   (ge.rsc:add-mesh-resource-bone bodge-mesh bodge-bone))
                 (loop for i below (bone :num-weights)
                       do (with-vertex-weight (weight (bone :weights * i))
                            (let* ((weight-info (cons bone-idx (weight :weight))))
                              (push weight-info (aref vertex-bone-array (weight :vertex-id))))))))
      (loop for i below vertex-count
            for offset = (* i +max-bones-per-vertex+)
            do (loop for j below +max-bones-per-vertex+
                     for (bone-id . weight) in (aref vertex-bone-array i)
                     for index = (+ offset j)
                     do (setf (aref bone-id-array index) bone-id
                              (aref bone-weight-array index) (ge.util:f weight))))
      (setf (ge.rsc:mesh-resource-bone-id-array bodge-mesh) bone-id-array
            (ge.rsc:mesh-resource-bone-weight-array bodge-mesh) bone-weight-array))))


(defun set-tex-coord-arrays (bodge-mesh mesh)
  (with-mesh (mesh)
    (loop with tex-coord-count = (mesh :num-vertices)
          for i below +max-tex-coord-channels+
          for tex-coord-array-ptr = (mesh :texture-coords i)
          for component-count = (mesh :num-uv-components i)
          when (> component-count 0)
            do (let ((attrib-array (make-attribute-array tex-coord-array-ptr
                                                         :float
                                                         tex-coord-count
                                                         component-count
                                                         (- 3 component-count))))
                 (setf (ge.rsc:mesh-resource-tex-coord-array bodge-mesh i) attrib-array)))))


(defun set-color-arrays (bodge-mesh mesh)
(with-mesh (mesh)
  (loop with color-count = (mesh :num-vertices)
        and component-count = 4
        for i below +max-color-channels+
        for color-array-ptr = (mesh :colors i)
        unless (claw:null-pointer-p color-array-ptr)
          do (let ((attrib-array (make-attribute-array color-array-ptr
                                                       :float
                                                       color-count
                                                       component-count)))
               (setf (ge.rsc:mesh-resource-color-array bodge-mesh i) attrib-array)))))


(defun make-mesh (mesh)
  (with-mesh (mesh)
    (let* ((primitive-type (switch ((mesh :primitive-types) :test #'=)
                             (%ai:+primitive-type-point+ :points)
                             (%ai:+primitive-type-line+ :lines)
                             (%ai:+primitive-type-triangle+ :triangles)
                             (t (error "Unexpected primitive type: ~A" (mesh :primitive-types)))))
           (bodge-mesh (ge.rsc:make-mesh-resource primitive-type)))
      (set-position-array bodge-mesh mesh)
      (set-index-array bodge-mesh mesh)
      (set-normal-array bodge-mesh mesh)
      (set-tangent-array bodge-mesh mesh)
      (set-color-arrays bodge-mesh mesh)
      (set-tex-coord-arrays bodge-mesh mesh)
      (set-skeleton bodge-mesh mesh)
      bodge-mesh)))


(defun fill-meshes (bodge-scene)
  (with-scene (scene)
    (loop for mesh-idx below (scene :num-meshes)
          for mesh = (make-mesh (scene :meshes * mesh-idx))
          do (setf (ge.rsc:scene-resource-mesh bodge-scene mesh-idx) mesh))))
