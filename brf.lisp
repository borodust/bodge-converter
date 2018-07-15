(cl:in-package :bodge-converter)


(defmacro with-character-stream ((stream &key (external-format :utf-8)) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :external-format ,external-format)))
     ,@body))


(defmacro with-binary-stream ((stream &key (element-type '(unsigned-byte 8))) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :element-type ',element-type)))
     ,@body))


(defun write-brf-magic (stream version)
  (with-binary-stream (stream)
    (format stream "bodged!")
    (write-byte version stream)))


(defmacro with-new-resource-file ((stream file-path &rest args &key &allow-other-keys)
                                        &body body)
  `(with-open-file (,stream ,file-path ,@args
                            :element-type '(unsigned-byte 8)
                            :direction :output)
     (with-standard-io-syntax
       (let ((*print-pretty* nil))
         (write-brf-magic ,stream 2)
         ,@body))))
