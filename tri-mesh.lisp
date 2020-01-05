;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)


(defclass plastic-vertex-shader (gl-shader)
  ((layout :initform
           '(
             ((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "normal")
              (:count . 3)
              (:type . :float))

             ((:name . "color")
              (:count . 4)
              (:type . :float))
             )
           :type (or null list))

   (source-file :initform (merge-pathnames *shader-dir* "plastic-vertex.glsl"))
   (shader-type :initform :vertex-shader)))

(defclass plastic-fragment-shader (gl-shader)
  ((shader-type :initform :fragment-shader)
   (source-file :initform (merge-pathnames *shader-dir* "plastic-fragment.glsl"))))

(defclass plastic-program (shader-program)
  ((shaders :initform (list
                       (make-instance 'plastic-vertex-shader)
                       (make-instance 'plastic-fragment-shader)))))


(defclass tri-mesh (vertex-object)
  ((vertices :initarg :vertices)
   (indices :initarg :indices)
   (shader-program :initform (make-instance 'plastic-program))
   (rotation :initform (/ pi 64))
   (frame :initform 0))
  (:documentation "A triangle mesh."))


(defmethod update ((object tri-mesh))
  )
