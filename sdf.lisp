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
  (let* ((atlas (sdf:make-atlas font-path pixel-size :width 300 :height 300))
         (image (sdf:atlas-image atlas))
         (image-data (flatten-array image)))
    (destructuring-bind (height width nil) (array-dimensions image)
      (with-character-stream (bodge-stream)
        (prin1 (list :image :width width :height height :pixel-format :rgb
                     :type :png :size (length image-data) :name (file-namestring font-path))
               bodge-stream)))
    (write-sequence image-data bodge-stream))
  t)
