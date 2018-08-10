(cl:in-package :bodge-converter)


(declaim (special *node-registry*))


(defstruct (node-registry
            (:constructor %make-node-registry))
  (id-counter -1 :type fixnum)
  (root nil)
  (table (make-hash-table :test #'equal)))


(defun make-node-registry ()
  (%make-node-registry))


(defun next-node-id ()
  (incf (node-registry-id-counter *node-registry*)))


(defun find-node-by-name (node-name)
  (gethash node-name (node-registry-table *node-registry*)))


(defun node-id (node)
  (ge.rsc:scene-resource-node-id node))


(defun node-property (node name)
  (ge.rsc:scene-resource-node-property node name))


(defun parse-node-metadata-entry (entry)
  (with-metadata-entry (entry)
    (eswitch ((entry :type) :test #'=)
      (%ai:+bool+ (/= (claw:c-ref (entry :data) :char) 0))
      (%ai:+int32+ (claw:c-ref (entry :data) :int))
      (%ai:+uint64+ (claw:c-ref (entry :data) :unsigned-long-long))
      (%ai:+float+ (claw:c-ref (entry :data) :float))
      (%ai:+double+ (claw:c-ref (entry :data) :double))
      (%ai:+aistring+ (ai-string-to-lisp (entry :data)))
      (%ai:+aivector3d+ (claw:c-let ((vec (:struct (%ai:vector3d)) :ptr (entry :data)))
                          (ge.ng:vec3 (vec :x) (vec :y) (vec :z)))))))


(defun parse-node-metadata (metadata)
  (with-metadata (metadata)
    (let ((table (make-hash-table :test #'equal)))
      (unless (claw:null-pointer-p metadata)
        (loop for i below (metadata :num-properties)
              for property-name = (ai-string-to-lisp (metadata :keys * i))
              for property-value = (parse-node-metadata-entry (metadata :values * i))
              do (setf (gethash property-name table) property-value)))
      table)))


(defun register-node (node)
  (with-node (node)
    (let* ((node-name (ai-string-to-lisp (node :name)))
           (node-table (node-registry-table *node-registry*))
           (node-id (next-node-id))
           (node-metadata (parse-node-metadata (node :meta-data)))
           (bodge-node (ge.rsc:make-scene-resource-node node-id (ai->mat4 (node :transformation)))))
      (when (gethash node-name node-table)
        (warn "Node with same name already registered: ~A" node-name))
      (maphash (lambda (key value)
                 (setf (ge.rsc:scene-resource-node-property bodge-node key) value))
               node-metadata)
      (loop for i below (node :num-meshes)
            do (ge.rsc:add-scene-resource-node-mesh-id bodge-node (node :meshes * i)))
      (setf (gethash node-name node-table) bodge-node
            (gethash node-id node-table) bodge-node)
      bodge-node)))


(defun register-scene-nodes ()
  (with-scene (scene)
    (labels ((%register-hierarchy (node)
               (with-node (node)
                 (let ((bodge-node (register-node node)))
                   (loop for i below (node :num-children)
                         for child = (%register-hierarchy (node :children * i))
                         do (ge.rsc:add-scene-resource-node-child bodge-node child))
                   bodge-node))))
      (setf (node-registry-root *node-registry*) (%register-hierarchy (scene :root-node))))))


(defun fill-nodes (bodge-scene)
  (setf (ge.rsc:scene-resource-root-node bodge-scene) (node-registry-root *node-registry*)))
