;;;; point-cloud.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass point-cloud (opengl-object)
  ((vertices :initform (make-array 0
                                   :element-type 'single-float
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (shader-program :initform
                   (make-shader-program
                    (shader-from-file (merge-pathnames *shader-dir* "point-vertex.glsl"))
                    (shader-from-file (merge-pathnames *shader-dir* "point-fragment.glsl"))))
   (transformation :initform (meye 4) :type mat4)
   (aspect-ratio :initarg :aspect-ratio :initform 1.0))
  (:documentation "Point cloud."))

(defmethod fill-buffers ((object point-cloud))
  (with-slots (vbos ebos vertices) object
    (cond ((null vbos)
           (setf vbos (gl:gen-buffers 1))
           (setf ebos (gl:gen-buffers 1))
           (let ((gl-vertices (to-gl-float-array vertices))
                 (gl-indices (to-gl-array (make-array
                                           (/ (length vertices) 7)
                                           :initial-contents (loop
                                                                for i
                                                                below (/ (length vertices)
                                                                         7)
                                                                collecting i)
                                           :element-type 'fixnum)
                                           :unsigned-int)
                                          ))

             (gl:bind-buffer :array-buffer (car vbos))
             (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car ebos))
             (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car vbos))
           (gl:bind-buffer :element-array-buffer (car ebos))))))
(defmethod render ((object opengl-object))
  (with-slots (vertices) object
    (with-slots (indices primitive-type) object
      (gl:draw-elements :points
                        (gl:make-null-gl-array :unsigned-int)
                        :count (/ (length vertices) 7)))))

(defun make-point-cloud ()
  (make-instance 'point-cloud))

(defun add-point (cloud &key x y z (red 0.0) (green 1.0) (blue 0.0) (alpha 1.0))
  (with-slots (vertices) cloud
    (vector-push-extend (coerce x 'single-float) vertices)
    (vector-push-extend (coerce y 'single-float) vertices)
    (vector-push-extend (coerce z 'single-float) vertices)
    (vector-push-extend (coerce red 'single-float) vertices)
    (vector-push-extend (coerce green 'single-float) vertices)
    (vector-push-extend (coerce blue 'single-float) vertices)
    (vector-push-extend (coerce alpha 'single-float) vertices)))
  
(defun random-point-cloud (&optional (n 100))
  (let ((pcloud (make-point-cloud)))
    (dotimes (i n)
      (add-point pcloud
                 :x (ju:random-between -1.0f0 1.0f0)
                 :y (ju:random-between -1.0f0 1.0f0)
                 :z (ju:random-between -1.0f0 1.0f0)
                 :red (ju:random-between 0.5f0 1.0f0)
                 :green (ju:random-between 0.0f0 1.0f0)
                 :blue (ju:random-between 0.0f0 1.0f0)
                 :alpha (ju:random-between 0.5f0 1.0f0)))
    pcloud))

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

