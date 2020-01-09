;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defun make-plastic-program ()
  (make-shader-program  (shader-from-file (merge-pathnames *shader-dir* "plastic-vertex.glsl"))
                        (shader-from-file (merge-pathnames *shader-dir* "plastic-fragment.glsl"))))

(defclass tri-mesh (vertex-object)
  ((vertices :initarg :vertices)
   (indices :initarg :indices)
   (shader-program :initform (make-plastic-program))
   (rotation :initform (/ pi 64))
   (frame :initform 0))
  (:documentation "A triangle mesh."))


(defmethod update ((object tri-mesh))
  )
