(cl:in-package :bodge-converter)


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
      bodge-mesh)))


(defun fill-meshes (bodge-scene)
  (with-scene (scene)
    (with-mesh (mesh-array (scene :meshes *))
      (loop for mesh-idx below (scene :num-meshes)
            for mesh = (make-mesh (mesh-array mesh-idx))
            do (setf (ge.rsc:scene-resource-mesh bodge-scene mesh-idx) mesh)))))


;;;
;;; MATERIALS
;;;
(defgeneric material-property-name (material))
(defgeneric init-material-property (property material))
(defgeneric apply-material-property (property bodge-material)
  (:method (property bodge-material)
    (declare (ignore bodge-material))
    (log:warn "~A is not applied" (material-property-name property))))


(defclass material-property ()
  ((name :initarg :name :initform (error ":name missing") :reader material-property-name)
   (semantic-type :initarg :semantic-type :initform (error ":semantic-type missing"))
   (index :initarg :index :initform (error ":index missing"))
   (value :initform nil :reader material-property-value)))

(defclass material-string-property (material-property) ())

(defmethod init-material-property ((this material-string-property) material)
  (with-slots (value name semantic-type index) this
    (claw:c-with ((str (:struct (%ai:string))))
      (%ai:get-material-string material name semantic-type index (str &))
      (setf value (ai-string-to-lisp str)))))

(defun read-float-array-property (material name semantic-type index expected-size)
  (let ((result (make-array expected-size :element-type 'single-float)))
    (claw:c-with ((actual-size :unsigned-int))
      (ge.util:with-simple-array-pointer (ptr result)
        (%ai:get-material-float-array material name semantic-type index ptr (actual-size &)))
      (unless (= expected-size actual-size)
        (error "Unexpected property size: expected ~A, but got ~A" expected-size actual-size))
      result)))

(defun read-integer-array-property (material name semantic-type index expected-size)
  (let ((result (make-array expected-size :element-type '(signed-byte 32))))
    (claw:c-with ((actual-size :unsigned-int))
      (ge.util:with-simple-array-pointer (ptr result)
        (%ai:get-material-integer-array material name semantic-type index ptr (actual-size &)))
      (unless (= expected-size actual-size)
        (error "Unexpected property size: expected ~A, but got ~A" expected-size actual-size))
      result)))

(defclass material-color3-property (material-property) ())

(defmethod init-material-property ((this material-color3-property) material)
  (with-slots (value name semantic-type index) this
    (claw:c-with ((color (:struct (%ai:color3d))))
      (%ai:get-material-color material name semantic-type index (color &))
      (setf value (ge.ng:vec3 (color :r) (color :g) (color :b))))))

(defclass material-vec4-property (material-property) ())

(defmethod init-material-property ((this material-vec4-property) material)
  (with-slots (value name semantic-type index) this
    (let ((array (read-float-array-property material name semantic-type index 4)))
      (setf value (ge.ng:sequence->vec4 array)))))

(defclass material-float-property (material-property) ())

(defmethod init-material-property ((this material-float-property) material)
  (with-slots (value name semantic-type index) this
    (let ((array (read-float-array-property material name semantic-type index 1)))
      (setf value (aref array 0)))))

(defclass material-bool-property (material-property) ())

(defmethod init-material-property ((this material-bool-property) material)
  (with-slots (value name semantic-type index) this
    (let ((array (read-integer-array-property material name semantic-type index 1)))
      (setf value (/= (aref array 0) 0)))))

(defclass material-integer-property (material-property) ())

(defmethod init-material-property ((this material-integer-property) material)
  (with-slots (value name semantic-type index) this
    (let ((array (read-integer-array-property material name semantic-type index 1)))
      (setf value (aref array 0)))))

(defun invoke-material-setter (property material setter)
  (with-slots (value) property
    (funcall setter value material)))

(defmacro define-material-property ((class name type &optional setter))
  (with-gensyms (this bodge-material)
    `(progn
       (defclass ,class (,type) ())
       (register-material-property-class ,name ',class)
       ,@(when setter
           `((defmethod apply-material-property ((,this ,class) ,bodge-material)
               (invoke-material-setter ,this ,bodge-material #'(setf ,setter))))))))
;;;
;;; COMMON MATERIAL PROPERTIES
;;;
(define-material-property (material-name "?mat.name" material-string-property
                                         ge.rsc:material-resource-name))

(define-material-property (material-shininess "$mat.shininess" material-float-property
                                              ge.rsc:material-resource-shininess))

;; Doesn't return value, see https://github.com/assimp/assimp/issues/1492
#++(define-material-property (material-two-sided "$mat.twosided") (material-bool-property))

;;;
;;; COLOR MATERIAL PROPERTIES
;;;
(define-material-property (material-color-diffuse "$clr.diffuse" material-color3-property
                                                  ge.rsc:material-resource-diffuse-color))

(define-material-property (material-color-emissive "$clr.emissive" material-color3-property
                                                   ge.rsc:material-resource-emissive-color))

;;;
;;; TEXTURE MATERIAL PROPERTIES
;;;
(defun ai->texture-type (semantic-type)
  (eswitch (semantic-type :test #'=)
    (%ai:+texture-type-diffuse+ :diffuse)
    (%ai:+texture-type-specular+ :specular)
    (%ai:+texture-type-ambient+ :ambient)
    (%ai:+texture-type-emissive+ :emissive)
    (%ai:+texture-type-height+ :height)
    (%ai:+texture-type-normals+ :normals)
    (%ai:+texture-type-shininess+ :shininess)
    (%ai:+texture-type-opacity+ :opacity)
    (%ai:+texture-type-displacement+ :displacement)
    (%ai:+texture-type-lightmap+ :lightmap)
    (%ai:+texture-type-reflection+ :reflection)
    (%ai:+texture-type-unknown+ :unknown)))

(defun apply-texture-property (property material setter)
  (with-slots (value index semantic-type) property
    (let ((tex (ge.rsc:material-resource-texture material (ai->texture-type semantic-type) index t)))
      (funcall setter value tex))))

(defmacro define-texture-property-applier ((property-type setter))
  (with-gensyms (this material)
    `(defmethod apply-material-property ((,this ,property-type) ,material)
       (apply-texture-property ,this ,material #'(setf ,setter)))))


(define-material-property (material-texture-file
                           "$tex.file"
                           material-string-property))
(defmethod apply-material-property ((this material-texture-file) material)
  (flet ((setter (value texture)
           (let ((name (fad:merge-pathnames-as-file *prefix* value)))
             (pushnew (list value name) *images*)
             (setf (ge.rsc:texture-resource-name texture) name))))
    (apply-texture-property this material #'setter)))


(defun ai->wrapping-mode (mode)
  (eswitch (mode :test #'=)
    (%ai:+texture-map-mode-wrap+ :wrap)
    (%ai:+texture-map-mode-clamp+ :clamp)
    (%ai:+texture-map-mode-decal+ :decal)
    (%ai:+texture-map-mode-mirror+ :mirror)))


(define-material-property (material-texture-map-mode-u
                           "$tex.mapmodeu"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-map-mode-u) material)
  (flet ((setter (value texture)
           (setf (ge.rsc:texture-resource-mapping-mode-u texture) (ai->wrapping-mode value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-map-mode-v
                           "$tex.mapmodev"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-map-mode-v) material)
  (flet ((setter (value texture)
           (setf (ge.rsc:texture-resource-mapping-mode-v texture) (ai->wrapping-mode value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-uvwsrc
                           "$tex.uvwsrc"
                           material-string-property))
(define-texture-property-applier (material-texture-uvwsrc
                                  ge.rsc:texture-resource-channel))


(define-material-property (material-texture-tex-coord
                           "$tex.file.texCoord"
                           material-integer-property))
(define-texture-property-applier (material-texture-tex-coord
                                  ge.rsc:texture-resource-coord-id))


(define-material-property (material-texture-mapping-name
                           "$tex.mappingname"
                           material-string-property))
(define-texture-property-applier (material-texture-mapping-name
                                  ge.rsc:texture-resource-mapping-name))


(define-material-property (material-texture-mapping-id
                           "$tex.mappingid"
                           material-string-property))
(define-texture-property-applier (material-texture-mapping-id
                                  ge.rsc:texture-resource-mapping-id))


(define-material-property (material-texture-mapping-filter-mag
                           "$tex.mappingfiltermag"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-mapping-filter-mag) material)
  (flet ((setter (value texture)
           (setf (ge.rsc:texture-resource-mapping-filter-mag texture) (ai->mapping-filter-type value))))
    (apply-texture-property this material #'setter)))


(defun ai->mapping-filter-type (type)
  (eswitch (type :test #'+)))

(define-material-property (material-texture-mapping-filter-min
                           "$tex.mappingfiltermin"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-mapping-filter-min) material)
  (flet ((setter (value texture)
           (setf (ge.rsc:texture-resource-mapping-filter-min texture) (ai->mapping-filter-type value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-scale
                           "$tex.scale"
                           material-float-property))
(define-texture-property-applier (material-texture-scale
                                  ge.rsc:texture-resource-scale))


(define-material-property (material-texture-strength
                           "$tex.strength"
                           material-float-property))
(define-texture-property-applier (material-texture-strength
                                  ge.rsc:texture-resource-strength))

;;;
;;; PBR MATERIAL PROPERTIES
;;;
(define-material-property (material-pbr-metallic-roughness-base-color-factor
                           "$mat.gltf.pbrMetallicRoughness.baseColorFactor"
                           material-vec4-property
                           ge.rsc:material-resource-base-color-factor))

(define-material-property (material-pbr-metallic-roughness-metallic-factor
                           "$mat.gltf.pbrMetallicRoughness.metallicFactor"
                           material-float-property
                           ge.rsc:material-resource-metallic-factor))

(define-material-property (material-pbr-metallic-roughness-roughness-factor
                           "$mat.gltf.pbrMetallicRoughness.roughnessFactor"
                           material-float-property
                           ge.rsc:material-resource-roughness-factor))

(define-material-property (material-pbr-metallic-roughness-glossiness-factor
                           "$mat.gltf.pbrMetallicRoughness.glossinessFactor"
                           material-float-property
                           ge.rsc:material-resource-glossiness-factor))

(define-material-property (material-pbr-alpha-mode
                           "$mat.gltf.alphaMode"
                           material-string-property))

(defmethod apply-material-property ((this material-pbr-alpha-mode) material)
  (with-slots (value) this
    (let ((mode (eswitch (value :test #'equalp)
                  ("opaque" :opaque)
                  ("mask" :mask)
                  ("blend" :blend))))
      (setf (ge.rsc:material-resource-alpha-mode material) mode))))


(define-material-property (material-pbr-alpha-cutoff
                           "$mat.gltf.alphaCutoff"
                           material-float-property
                           ge.rsc:material-resource-alpha-cutoff))

(define-material-property (material-pbr-specular-glossiness
                           "$mat.gltf.pbrSpecularGlossiness"
                           material-bool-property))

(define-material-property (material-pbr-unlit
                           "$mat.gltf.unlit"
                           material-bool-property))


(defun set-property (bodge-material material property)
  (with-material-property (property)
    (let ((property-name (ai-string-to-lisp (property :key))))
      (if-let ((property-class (find-material-property-class property-name)))
        (let ((instance (make-instance property-class :name property-name
                                                      :semantic-type (property :semantic)
                                                      :index (property :index))))
          (init-material-property instance material)
          (apply-material-property instance bodge-material))
        (log:warn "Ignoring property ~A" property-name)))))


(defun fill-material (bodge-material material)
  (with-material (material)
    (loop for i from 0 below (material :num-properties)
          do (set-property bodge-material material (material :properties * i)))))


(defun fill-materials (bodge-scene)
  (with-scene (scene)
    (loop for i from 0 below (scene :num-materials)
          for material = (ge.rsc:make-material-resource)
          do (setf (ge.rsc:scene-resource-material bodge-scene i) material)
             (fill-material material (scene :materials * i))
          collect material)))

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
    (let* ((scene (ge.rsc:make-empty-scene-resource))
           (*prefix* (or prefix "/"))
           (*images* (list))
           (data (flex:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                   (fill-meshes scene)
                   (fill-materials scene)
                   (ge.rsc:encode-resource (ge.rsc:make-resource-handler :scene) scene out))))
      (ge.rsc:write-chunk bodge-stream :scene
                          (fad:merge-pathnames-as-file *prefix* scene-name)
                          data)
      (loop for (relative-path name) in *images*
            do (write-image bodge-stream (fad:merge-pathnames-as-file
                                          (fad:pathname-directory-pathname scene-file)
                                          relative-path)
                            :prefix (fad:pathname-directory-pathname name)
                            :image-name (file-namestring name)
                            :type :png))
      scene)))
