(cl:in-package :bodge-converter)

(defparameter *default-characters*
  (format nil "~{~A~}" (list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                             "0123456789.,;:?!@#$%^&*()-+_<>'\"`~{}[]/\\| ")))


(defun write-sdf-font (bodge-stream font-path pixel-size &key width height prefix font-name)
  (let* ((prefix (merge-pathnames (uiop:ensure-directory-pathname (or prefix "/")) #p"/"))
         (name (or (fad:pathname-as-file font-name) (file-namestring font-path)))
         (image-name (format nil "~A~A/image" prefix name))
         (font-atlas-name (format nil "~A~A/font" prefix name))
         (atlas (sdf:make-atlas font-path pixel-size
                                :width width :height height
                                :string *default-characters*)))
    (%png-image-to-bodge bodge-stream (sdf:atlas-image atlas) image-name)

    (let* ((font (sdf:atlas-metrics atlas))
           (glyphs (loop for g in (sdf:font-glyphs font)
                         for ch = (sdf:glyph-character g)
                         collect (ge.rsc:make-glyph-metrics-resource
                                  :character (char-code ch)
                                  :bounding-box (sdf:glyph-bounding-box g)
                                  :origin (sdf:glyph-origin g)
                                  :advance-width (sdf:glyph-advance-width g)
                                  :kernings '())))
           (font-chunk (ge.rsc:make-font-atlas-resource :image-name "image" ; relative
                                                        :ascender (sdf:font-ascender font)
                                                        :descender (sdf:font-descender font)
                                                        :line-gap (sdf:font-line-gap font)
                                                        :glyphs glyphs))
           (data (flex:with-output-to-sequence (stream)
                   (ge.rsc:encode-resource (ge:make-resource-handler :font-atlas) font-chunk stream))))
      (ge.rsc:write-chunk bodge-stream :font-atlas font-atlas-name data)))
  t)


(defun dump-sdf-atlas (font-path atlas-path metrics-path pixel-size &key width height)
  (let ((atlas (sdf:make-atlas font-path pixel-size
                               :width width :height height
                               :string *default-characters*)))
    (sdf:save-atlas atlas atlas-path metrics-path)))
