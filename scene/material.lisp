(cl:in-package :bodge-converter)

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

(defclass material-uv-transform-property (material-property) ())

(defmethod init-material-property ((this material-uv-transform-property) material)
  (with-slots (value name semantic-type index) this
    (claw:c-with ((transform (:struct (%ai:uv-transform))))
      (%ai:get-material-uv-transform material name semantic-type index (transform &))

      (setf value (ge.ng:mult (ge.ng:euler-angle->mat3 (transform :rotation))
                              (ge.ng:translation-mat3 (transform :translation :x)
                                                      (transform :translation :y))
                              (ge.ng:scaling-mat3 (transform :scaling :x)
                                                  (transform :scaling :y)))))))


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

(define-material-property (material-shininess-strength "$mat.shinpercent" material-float-property
                                                       ge.rsc:material-resource-shininess-strength))

(define-material-property (material-opacity "$mat.opacity" material-float-property
                                              ge.rsc:material-resource-opacity))

(define-material-property (material-reflectivity "$mat.reflectivity" material-float-property
                                              ge.rsc:material-resource-reflectivity))

(define-material-property (material-bump-scaling "$mat.bumpscaling" material-float-property
                                                 ge.rsc:material-resource-bump-scaling))

(define-material-property (material-displacement-scaling "$mat.displacementscaling" material-float-property
                                                         ge.rsc:material-resource-displacement-scaling))

;; Doesn't return value, see https://github.com/assimp/assimp/issues/1492
#++(define-material-property (material-two-sided "$mat.twosided") (material-bool-property))

;;;
;;; COLOR MATERIAL PROPERTIES
;;;
(define-material-property (material-color-diffuse "$clr.diffuse" material-color3-property
                                                  ge.rsc:material-resource-diffuse-color))

(define-material-property (material-color-ambient "$clr.ambient" material-color3-property
                                                  ge.rsc:material-resource-ambient-color))

(define-material-property (material-color-specular "$clr.specular" material-color3-property
                                                  ge.rsc:material-resource-specular-color))

(define-material-property (material-color-emissive "$clr.emissive" material-color3-property
                                                   ge.rsc:material-resource-emissive-color))

(define-material-property (material-color-transparent "$clr.transparent" material-color3-property
                                                      ge.rsc:material-resource-transparent-color))

(define-material-property (material-color-reflective "$clr.reflective" material-color3-property
                                                     ge.rsc:material-resource-reflective-color))

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
           (let ((name (ge.util:replace-all (unixify value) "../" "--/")))
             (pushnew (list name (unixify value)) *images* :test #'equal)
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
                           material-integer-property))
(define-texture-property-applier (material-texture-uvwsrc
                                  ge.rsc:texture-resource-channel))


(define-material-property (material-texture-uv-transform
                           "$tex.uvtrafo"
                           material-uv-transform-property))
(define-texture-property-applier (material-texture-uv-transform
                                  ge.rsc:texture-resource-uv-transform))


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
  (eswitch (type :test #'=)
    (9728 :nearest)
    (9729 :linear)
    (9984 :nearest-mipmap-nearest)
    (9985 :linear-mipmap-nearest)
    (9986 :nearest-mipmap-linear)
    (9987 :linear-mipmap-linear)))


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
        (warn "Ignoring property ~A" property-name)))))


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
