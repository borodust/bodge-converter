(cl:in-package :bodge-converter)

(defparameter *default-characters*
  (format nil "~{~A~}" (list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                             "0123456789.,;:?!@#$%^&*()-+_<>'\"`~{}[]/\\| ")))


(defun write-sdf-font (bodge-stream font-path pixel-size &key width height (resource-prefix "/sdf/"))
  (let* ((prefix (merge-pathnames (uiop:ensure-directory-pathname resource-prefix) #p"/"))
         (name (file-namestring font-path))
         (image-name (format nil "~A~A/image" prefix name))
         (font-atlas-name (format nil "~A~A/font" prefix name))
         (atlas (sdf:make-atlas font-path pixel-size
                                :width width :height height
                                :string *default-characters*)))
    (%png-image-to-bodge bodge-stream (sdf:atlas-image atlas) image-name)
    (let* ((font (sdf:atlas-metrics atlas))
           (font-chunk (list (list font-atlas-name
                                   :image-name image-name
                                   :ascender (sdf:font-ascender font)
                                   :descender (sdf:font-descender font)
                                   :line-gap (sdf:font-line-gap font))))
           (glyphs (loop for g in (sdf:font-glyphs font)
                         for ch = (sdf:glyph-character g)
                         collect (list (format nil "~a:~a" name ch)
                                       :character (char-code ch)
                                       :bounding-box (sdf:glyph-bounding-box g)
                                       :origin (sdf:glyph-origin g)
                                       :advance-width (sdf:glyph-advance-width g)
                                       :kernings '())))
           (data (flex:with-output-to-sequence (stream)
                   (with-character-stream (stream)
                     (prin1 (append font-chunk glyphs) stream)))))
      (with-character-stream (bodge-stream)
        (prin1 (list :font-atlas :name font-atlas-name :size (length data))
               bodge-stream))
      (write-sequence data bodge-stream)))
  t)


(defun dump-sdf-atlas (font-path atlas-path metrics-path pixel-size &key width height)
  (let ((atlas (sdf:make-atlas font-path pixel-size
                               :width width :height height
                               :string *default-characters*)))
    (sdf:save-atlas atlas atlas-path metrics-path)))
