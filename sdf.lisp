(in-package :bodge-converter)


(defun flatten-array (array)
  (let* ((dims (array-dimensions array)))
    (labels ((total-offset (offsets sizes)
               (if offsets
                   (+ (* (first offsets) (reduce #'* sizes))
                      (total-offset (rest offsets) (rest sizes)))
                   0))
             (copy-dimension (dst src offset rest-dims)
               (if (null rest-dims)
                   (let ((dst-offset (total-offset offset (rest dims))))
                     (setf (aref dst dst-offset) (apply #'aref src offset)))
                   (loop with size = (first rest-dims)
                      for i from 0 below size
                      do (copy-dimension dst src (append offset (list i)) (rest rest-dims))))))

      (if (null (cdr dims))
          array
          (let* ((result (make-array (reduce #'* dims)
                                     :element-type (array-element-type array))))
            (copy-dimension result array '() dims)
            result)))))


(defun sdf-to-bodge (bodge-stream font-path pixel-size)
  (let* ((name (file-namestring font-path))
         (atlas (sdf:make-atlas font-path pixel-size :width 305 :height 305))
         (image (opticl:transform-image (sdf:atlas-image atlas)
                                        (opticl:make-affine-transformation :y-scale -1.0)))
         (image-data (flatten-array image)))
    (destructuring-bind (height width nil) (array-dimensions image)
      (with-character-stream (bodge-stream)
        (prin1 (list :image :width width :height height :pixel-format :rgb
                     :type :raw :size (length image-data) :name name)
               bodge-stream)))
    (write-sequence image-data bodge-stream)

    (with-character-stream (bodge-stream)
      (prin1 '(:font-atlas) bodge-stream)
      (let* ((font (sdf:atlas-metrics atlas))
             (font-chunk (list (list name
                                     :image-name name
                                     :ascender (sdf:font-ascender font)
                                     :descender (sdf:font-descender font)
                                     :line-gap (sdf:font-line-gap font))))
             (glyphs (loop for g in (sdf:font-glyphs font)
                        for ch = (sdf:glyph-character g)
                        collect (list (format nil "~a:~a" name ch)
                                 :character ch
                                 :bounding-box (sdf:glyph-bounding-box g)
                                 :origin (sdf:glyph-origin g)
                                 :advance-width (sdf:glyph-advance-width g)
                                 :kernings '()))))
        (prin1 (append font-chunk glyphs) bodge-stream))))
  t)
