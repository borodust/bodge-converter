(in-package :bodge-converter)


(defun channels->pixel-format (channels)
  (ecase channels
    (1 :grey)
    (3 :rgb)
    (4 :rgba)))


(defun %raw-image-to-bodge (bodge-stream image image-name)
  (let* ((image (opticl:transform-image image
                                        (opticl:make-affine-transformation :y-scale -1.0)))
         (image-data (flatten-array image))
         (compressed-data (salza2:compress-data image-data 'salza2:deflate-compressor)))
    (destructuring-bind (height width channels) (array-dimensions image)
      (with-character-stream (bodge-stream)
        (prin1 (list :image :width width :height height
                     :pixel-format (channels->pixel-format channels)
                     :type :raw :size (length compressed-data) :name image-name
                     :compression :deflate :uncompressed-size (length image-data))
               bodge-stream)))
    (write-sequence compressed-data bodge-stream))
  t)


(defun %png-image-to-bodge (bodge-stream image image-name)
  (let* ((image (opticl:transform-image image
                                        (opticl:make-affine-transformation :y-scale -1.0)))
         (image-data (flex:with-output-to-sequence (out)
                       (opticl:write-png-stream out image))))
    (with-character-stream (bodge-stream)
      (prin1 (list :image :type :png :size (length image-data) :name image-name)
             bodge-stream))
    (write-sequence image-data bodge-stream))
  t)


(defun image-to-bodge (bodge-stream image-path &key ((:image-name custom-image-name))
                                                 (type :raw))
  (let ((image (opticl:read-image-file image-path))
        (name (or custom-image-name (format nil "/image/~A" (file-namestring image-path)))))
    (case type
      (:raw (%raw-image-to-bodge bodge-stream image name))
      (:png (%png-image-to-bodge bodge-stream image name)))))
