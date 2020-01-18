;;;; point-cloud.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass point-cloud (vertex-object)
  ((vertices :initform (make-array 0
                                   :element-type 'single-float
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (indices :initform (make-array 0
                                  :element-type 'fixnum
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0))
   (primitive-type :initform :points)
   (shader-program :initform
                   (make-shader-program
                    (shader-from-file (merge-pathnames *shader-dir* "point-vertex.glsl"))
                    (shader-from-file (merge-pathnames *shader-dir* "point-fragment.glsl"))))
   (transformation :initform (meye 4) :type mat4)
   (aspect-ratio :initarg :aspect-ratio :initform 1.0))
  (:documentation "Point cloud."))

(defun make-point-cloud ()
  (make-instance 'point-cloud))

(defun add-point (cloud x y z)
  (with-slots (vertices indices) cloud
    (let ((index  (length vertices)))
      (vector-push-extend (coerce x 'single-float) vertices)
      (vector-push-extend (coerce y 'single-float) vertices)
      (vector-push-extend (coerce z 'single-float) vertices)
      (vector-push-extend index indices))))

(defun add-point-color (cloud x y z red green blue alpha)
  (with-slots (vertices indices) cloud
    (let ((index  (length indices)))
      (vector-push-extend (coerce x 'single-float) vertices)
      (vector-push-extend (coerce y 'single-float) vertices)
      (vector-push-extend (coerce z 'single-float) vertices)
      (vector-push-extend (coerce red 'single-float) vertices)
      (vector-push-extend (coerce green 'single-float) vertices)
      (vector-push-extend (coerce blue 'single-float) vertices)
      (vector-push-extend (coerce alpha 'single-float) vertices)
      (vector-push-extend index indices))))
  

(defmethod set-uniforms ((object point-cloud))
  (call-next-method)
  (with-slots (shader-program transformation aspect-ratio) object
    (set-uniform shader-program "transform" (m* (3d-matrices:mscaling (vec3 1.0 aspect-ratio 1.0))
                                                transformation))))

(defmethod handle-key ((object point-cloud) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (call-next-method))

(defmethod handle-resize ((object point-cloud) window width height)
  (with-slots (aspect-ratio) object
    (setf aspect-ratio (if (< height width )
                           (/ width height 1.0)
                           (/ height width -1.0))))
  (set-uniforms object))

