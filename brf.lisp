(in-package :bodge-converter)


(defmacro with-character-stream ((stream &key (external-format :utf-8)) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :external-format ,external-format)))
     ,@body))


(defmacro with-new-resource-file ((stream file-path &rest args &key &allow-other-keys)
                                        &body body)
  `(with-open-file (,stream ,file-path ,@args
                            :element-type '(unsigned-byte 8)
                            :direction :output)
     (let ((*print-pretty* nil))
       (with-character-stream (,stream)
         (prin1 '(:brf 1) ,stream))
       ,@body)))
