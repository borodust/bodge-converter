(cl:in-package :bodge-converter)


(defun channels->pixel-format (channels)
  (ecase channels
    (1 :grey)
    (3 :rgb)
    (4 :rgba)))


(defun %raw-image-to-bodge (bodge-stream image image-name)
  (destructuring-bind (height width channels) (array-dimensions image)
    (let* ((image (opticl:transform-image image
                                          (opticl:make-affine-transformation :y-scale -1.0)))
           (image-data (ge.util:flatten-array image))
           (data (flex:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                   (with-character-stream (out)
                     (prin1 (list :header :width width :height height
                                          :pixel-format (channels->pixel-format channels)
                                          :type :raw)
                            out))
                   (write-sequence image-data out)))
           (compressed-data (salza2:compress-data data 'salza2:zlib-compressor)))
      (with-character-stream (bodge-stream)
        (prin1 (list :image :size (length compressed-data)
                            :name (fad:merge-pathnames-as-file image-name)
                            :compression :zlib)
               bodge-stream))
    (write-sequence compressed-data bodge-stream)))
  t)


(defun %png-image-to-bodge (bodge-stream image image-name)
  (let* ((image-data (flex:with-output-to-sequence (out)
                       (opticl:write-png-stream out image))))
    (with-character-stream (bodge-stream)
      (prin1 (list :image :size (length image-data) :name image-name)
             bodge-stream))
    (write-sequence image-data bodge-stream))
  t)


(defun write-image (bodge-stream image-path &key ((:image-name custom-image-name))
                                              prefix
                                              (type :raw))
  (with-standard-io-syntax
    (let* ((image (opticl:read-image-file image-path))
           (image-path (fad:merge-pathnames-as-file image-path))
           (name (ensure-bodge-name (or custom-image-name (file-namestring image-path))
                                    (or prefix (fad:pathname-directory-pathname image-path))))
           (*print-pretty* nil))
      (ecase type
        (:raw (%raw-image-to-bodge bodge-stream image name))
        (:png (%png-image-to-bodge bodge-stream image name))))))
