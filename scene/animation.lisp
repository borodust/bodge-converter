(cl:in-package :bodge-converter)


(declaim (special *animation-ticks-per-second*))


(defun ai->animation-state (state)
  (eswitch (state :test #'=)
    (%ai:+i-anim-behaviour-default+ :default)
    (%ai:+i-anim-behaviour-constant+ :constant)
    (%ai:+i-anim-behaviour-repeat+ :repeat)
    (%ai:+i-anim-behaviour-linear+ :linear)))


(defun extract-keys (key-count extractor)
  (labels ((extract-value (index tag)
             (funcall extractor index tag))
           (find-next-key (i)
             (let* ((current (extract-value i :key))
                    (next-index (loop for j from (1+ i) below key-count
                                      for next = (extract-value j :key)
                                      while (equal current next)
                                      finally (return (1- j)))))
               (if (= i next-index)
                   (1+ i)
                   next-index))))
    (let* ((timing-array (make-array 0 :element-type 'single-float
                                       :fill-pointer t :adjustable t))
           (key-array (make-array 0 :element-type 'single-float
                                    :fill-pointer t :adjustable t)))
      (loop for i = 0 then (find-next-key i)
            while (< i key-count)
            do (vector-push-extend
                (ge.util:f (/ (extract-value i :time) *animation-ticks-per-second*)) timing-array)
               (loop for key in (extract-value i :key)
                     do (vector-push-extend (ge.util:f key) key-array)))
      (ge.rsc:make-animation-resource-key-sequence
       :timing-array (make-array (length timing-array)
                                 :element-type 'single-float
                                 :initial-contents timing-array)
       :value-array (make-array (length key-array)
                                :element-type 'single-float
                                :initial-contents key-array)))))


(defun extract-vector-keys (count keys)
  (with-vector-key (keys)
    (extract-keys count (lambda (index tag)
                          (eswitch (tag :test #'equal)
                            (:time (keys index :time))
                            (:key (list (keys index :value :x)
                                        (keys index :value :y)
                                        (keys index :value :z))))))))


(defun extract-quat-keys (count keys)
  (with-quat-key (keys)
    (extract-keys count (lambda (index tag)
                          (eswitch (tag :test #'equal)
                            (:time (keys index :time))
                            (:key (list (keys index :value :x)
                                        (keys index :value :y)
                                        (keys index :value :z)
                                        (keys index :value :w))))))))


(defun make-animation-channel (channel)
  (with-animation-node (channel)
    (ge.rsc:make-animation-resource-channel
     (node-id (find-node-by-name (ai-string-to-lisp (channel :node-name))))
     (ai->animation-state (channel :pre-state))
     (ai->animation-state (channel :post-state))
     :rotation-sequence (extract-quat-keys (channel :num-rotation-keys) (channel :rotation-keys))
     :translation-sequence (extract-vector-keys (channel :num-position-keys) (channel :position-keys))
     :scale-sequence (extract-vector-keys (channel :num-scaling-keys) (channel :scaling-keys)))))


(defun make-animation (animation)
  (with-animation (animation)
    (let* ((configured-ticks-per-second (animation :ticks-per-second))
           (*animation-ticks-per-second* (if  (> configured-ticks-per-second 0)
                                              configured-ticks-per-second
                                              1))
           (bodge-animation (ge.rsc:make-animation-resource
                             (/ (animation :duration) *animation-ticks-per-second*))))
      (loop for idx below (animation :num-channels)
            for channel = (make-animation-channel (animation :channels * idx))
            do (ge.rsc:add-animation-resource-channel bodge-animation channel))
      bodge-animation)))


(defun fill-animations (bodge-scene)
  (with-scene (scene)
    (loop for idx below (scene :num-animations)
          do (let ((animation (make-animation (scene :animations * idx))))
               (setf (ge.rsc:scene-resource-animation bodge-scene idx) animation)))))
