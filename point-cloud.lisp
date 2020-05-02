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

(defun add-point-pc (cloud &key x y z (red 1.0f0) (green 1.0f0)  (blue 1.0f0) (alpha 1.0f0))
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
      (add-point-pc pcloud
                 :x (ju:random-between -0.64f0 0.64f0)
                 :y (ju:random-between -0.64f0 0.64f0)
                 :z (ju:random-between -0.64f0 0.64f0)
                 :red (ju:random-between 0.64f0 1.0f0)
                 :green (ju:random-between 0.0f0 1.0f0)
                 :blue (ju:random-between 0.0f0 1.0f0)
                 :alpha (ju:random-between 0.5f0 1.0f0)))
    pcloud))



(defmethod handle-key ((object point-cloud) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (call-next-method))

(defmethod handle-resize ((object line-segments) window width height)
  (with-slots (xform) object
    (setf xform (3d-matrices:mscaling
                 (if (< height width )
                     (3d-vectors:vec3 (/ height width 1.0) 1.0 1.0)
                     (3d-vectors:vec3 1.0 (/ width height  1.0) 1.0))))
    (when *debug-stream* (format *debug-stream* "Transform: ~a~%" xform)))
  (set-uniforms object))

(defun parametric-point-cloud ()
  (let* ((pc (make-point-cloud))
         (i-steps 180)
         (j-steps 180)
         (u-min (- pi))
         (v-min (- pi))
         (du (/ (* 2 pi) i-steps))
         (dv (/ (* 2 pi) j-steps)))
    (dotimes (i i-steps)
      (let ((uv (+ u-min (* i du))))
        (dotimes (j j-steps)
          (let* ((vv (+ v-min (* j dv)))
                 (xv uv)
                 (yv (* 3.0 (sin uv) (cos vv)))
                 (zv vv))
            (add-point-pc pc :x xv :y yv :z zv)))))
    pc))

;; (newgl:viewer pc
;;               :xform
;;               (3d-matrices:m* (3d-matrices:mscaling (3d-vectors:vec3 scale scale scale))
;;                               (3d-matrices:mrotation (3d-vectors:vec3 1.0 0.0 0.0) (/ pi 3))
;;                               (3d-matrices:mrotation (3d-vectors:vec3 0.0 1.0 0.0) (/ pi 3)))))
