;; primitives.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass tri-mesh (geometry)
  ((vertices :initarg :vertices)
   (indices :initarg :indices)
   (shader-program :initform (make-plastic-program))
   (rotation :initform (/ pi 64))
   (frame :initform 0))
  (:documentation "A triangle mesh."))

(defmethod vertex-buffers ((object tri-mesh))
  (with-slots (vertices indices) object
    (values vertices indices)))

(defun add-point-tm (tm &key x y z (red 1.0f0) (green 1.0f0)  (blue 1.0f0) (alpha 1.0f0))
  (with-slots (vertices indices) tm
    (let ((index  (length indices)))
      (vector-push-extend (coerce x 'single-float) vertices)
      (vector-push-extend (coerce y 'single-float) vertices)
      (vector-push-extend (coerce z 'single-float) vertices)
      (vector-push-extend (coerce red 'single-float) vertices)
      (vector-push-extend (coerce green 'single-float) vertices)
      (vector-push-extend (coerce blue 'single-float) vertices)
      (vector-push-extend (coerce alpha 'single-float) vertices)
      (vector-push-extend index indices))))

(defun parametric-tri-mesh ()
  (let* ((pc (make-instance 'tri-mesh
                            :vertices (make-array 0
                                                  :element-type 'single-float
                                                  :initial-contents '()
                                                  :adjustable t
                                                  :fill-pointer 0)
                            :indices (make-array 0
                                                 :element-type 'fixnum
                                                 :initial-contents '()
                                                 :adjustable t
                                                 :fill-pointer 0)))
         ;; (scale (/ 1 (* 2 pi)))
         (i-steps 10)
         (j-steps 10)
         (u-min (- pi))
         (v-min (- pi))
         (du (/ (* 2 pi) i-steps))
         (dv (/ (* 2 pi) j-steps)))
    (labels ((fx (uv vv)
               (declare (ignorable uv vv))
               uv)
             (fy (uv vv)
               (declare (ignorable uv vv))
               (* 3.0 (sin uv) (cos vv)))
             (fz (uv vv)
               (declare (ignorable uv vv))
               vv)
             (tm-add-point (uv vv)
               (let ((xv (fx uv vv))
                     (yv (fy uv vv))
                     (zv (fz uv vv)))
                 (add-point-tm pc :x xv :y yv :z zv))))
      (dotimes (i i-steps)
        (let ((uv (+ u-min (* i du))))
          (dotimes (j j-steps)
            (let* ((vv (+ v-min (* j dv))))
              (tm-add-point (+ uv du) vv)
              (tm-add-point uv vv)
              (tm-add-point uv (+ vv dv))
              (tm-add-point uv (+ vv dv))
              (tm-add-point (+ uv du) (+ vv dv))
              (tm-add-point (+ uv du) vv))
              ))))
  pc))
