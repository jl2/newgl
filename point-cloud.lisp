;; point-cloud.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass point-cloud (geometry)
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
   (shaders :initform (list (shader-from-file (newgl-shader "color-position-vertex.glsl"))
                            (shader-from-file (newgl-shader "point-fragment.glsl")))))
  (:documentation "Point cloud."))

(defmethod allocate-and-fill-buffers ((object point-cloud))
  (with-slots (vertices indices) object
    (values (to-gl-float-array vertices) (to-gl-array indices :unsigned-int))))


(defun make-point-cloud ()
  (make-instance 'point-cloud))

(defun add-point-pc (cloud x y z red green blue alpha)
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

(defun add-point (cloud pt color)
  (with-slots (vertices indices) cloud
    (let ((index  (length indices)))
      (vector-push-extend (vx pt) vertices)
      (vector-push-extend (vy pt) vertices)
      (vector-push-extend (vz pt) vertices)
      (vector-push-extend (vx color) vertices)
      (vector-push-extend (vy color) vertices)
      (vector-push-extend (vz color) vertices)
      (vector-push-extend (vw color) vertices)
      (vector-push-extend index indices))))

(defun random-point-cloud (&optional (n 100))
  (let ((pcloud (make-point-cloud)))
    (dotimes (i n)
      (add-point pcloud
                 (vec3-random -0.64f0 0.64f0)
                 (vec4-random 0.2f0 1.0f0)))
    pcloud))



(defmethod handle-key ((object point-cloud) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (call-next-method))

(defmethod handle-resize ((object point-cloud) window width height)
  (declare (ignorable window width height))
  (call-next-method))

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
            (add-point-pc pc xv yv zv 0.0 1.0 0.0 0.0)))))
    pc))
