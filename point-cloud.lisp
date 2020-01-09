;;;; point-cloud.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass point-cloud (newgl:vertex-object)
  ((newgl:vertices :initarg :vertices :initform (make-array 0
                                                            :element-type 'single-float
                                                            :initial-contents '()
                                                            :adjustable t
                                                            :fill-pointer 0))
   (newgl:indices :initarg :indices :initform (make-array 0
                                                          :element-type 'fixnum
                                                          :initial-contents '()
                                                          :adjustable t
                                                          :fill-pointer 0))
   (transformation :initform (meye 4) :type mat4)
   (aspect-ratio :initarg :aspect-ratio :initform 1.0))
  (:documentation "Point cloud."))

(defun add-point (cloud x y z)
  (with-slots (newgl:vertices newgl:indices) cloud
    (let ((index  (length newgl:vertices)))
      (vector-push-extend (coerce x 'single-float) newgl:vertices)
      (vector-push-extend (coerce y 'single-float) newgl:vertices)
      (vector-push-extend (coerce z 'single-float) newgl:vertices)
      (vector-push-extend index newgl:indices))))
  

(defmethod newgl:set-uniforms ((object point-cloud))
  (call-next-method)
  (with-slots (newgl:shader-program transformation aspect-ratio) object
    (let ((xform-location (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "transformationMatrix")))
      (gl:uniform-matrix xform-location
                         4
                         (vector (3d-matrices:marr4 transformation))
                         t))))

(defmethod newgl:handle-key ((object point-cloud) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (call-next-method)
  )

(defmethod newgl:handle-resize ((object point-cloud) window width height)
  (with-slots (aspect-ratio) object
    (setf aspect-ratio (if (< height width )
                           (/ width height 1.0)
                           (/ height width -1.0)))))

