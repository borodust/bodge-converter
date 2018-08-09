(cl:in-package :bodge-converter)


(defmacro with-character-stream ((stream &key (external-format :utf-8)) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :external-format ,external-format)))
     ,@body))


(defmacro with-binary-stream ((stream &key (element-type '(unsigned-byte 8))) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :element-type ',element-type)))
     ,@body))


(defmacro with-new-resource-file ((stream file-path &rest args &key &allow-other-keys)
                                  &body body)
  `(with-open-file (,stream ,file-path ,@args
                            :element-type '(unsigned-byte 8)
                            :direction :output)
     (ge.rsc:write-brf-magic ,stream 2)
     (with-standard-io-syntax
       (let ((*print-pretty* nil))
         ,@body))))


(defun ensure-bodge-name (name &optional prefix)
  (unless name
    (error "Name cannot be null"))
  (fad:merge-pathnames-as-file (or prefix "/") name))
