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
   (aspect-ratio :initarg :aspect-ratio :initform 1.0))
  (:documentation "Point cloud."))

(defun make-point-cloud ()
  (make-instance 'point-cloud))

(defun add-point (cloud &key x y z (red 1.0f0) (green 1.0f0)  (blue 1.0f0) (alpha 1.0f0))
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

(defun random-point-cloud (&optional (n 100))
  (let ((pcloud (make-point-cloud)))
    (dotimes (i n)
      (add-point pcloud
                 :x (ju:random-between -0.25f0 0.25f0)
                 :y (ju:random-between -0.25f0 0.25f0)
                 :z (ju:random-between -0.25f0 0.25f0)
                 :red (ju:random-between 0.25f0 1.0f0)
                 :green (ju:random-between 0.0f0 1.0f0)
                 :blue (ju:random-between 0.0f0 1.0f0)
                 :alpha (ju:random-between 0.5f0 1.0f0)))
    pcloud))



(defmethod handle-key ((object point-cloud) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (call-next-method))

(defmethod handle-resize ((object point-cloud) window width height)
  (with-slots (aspect-ratio) object
    (setf aspect-ratio (if (< height width )
                           (/ width height 1.0)
                           (/ height width 1.0))))
  (set-uniforms object))

(defun make-square ()
  (let ((pc (newgl:make-point-cloud)))
    (newgl:add-point pc :x 0.0 :y 0.0 :z 0.0)
    (newgl:add-point pc :x 1.0 :y 0.0 :z 0.0)
    (newgl:add-point pc :x 1.0 :y 1.0 :z 0.0)
    (newgl:add-point pc :x 0.0 :y 1.0 :z 0.0)
    pc))
