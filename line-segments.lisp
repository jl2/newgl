;;;; point-cloud.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass line-segments (vertex-object)
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
   (primitive-type :initform :lines)
   (shader-program :initform
                   (make-shader-program
                    (shader-from-file (merge-pathnames *shader-dir* "point-vertex.glsl"))
                    (shader-from-file (merge-pathnames *shader-dir* "point-fragment.glsl"))))
   (aspect-ratio :initarg :aspect-ratio :initform 1.0))
  (:documentation "Point cloud."))

(defun make-line-segments ()
  (make-instance 'line-segments))

(defun add-line (lines &key
                         x1 y1 z1
                         x2 y2 z2
                         (r1 1.0f0) (g1 1.0f0) (b1 1.0f0) (a1 1.0f0)
                         (r2 r1) (g2 g1) (b2 b1) (a2 a1))
  (with-slots (vertices indices) lines
    (let ((index  (length indices)))
      (vector-push-extend (coerce x1 'single-float) vertices)
      (vector-push-extend (coerce y1 'single-float) vertices)
      (vector-push-extend (coerce z1 'single-float) vertices)
      (vector-push-extend (coerce r1 'single-float) vertices)
      (vector-push-extend (coerce g1 'single-float) vertices)
      (vector-push-extend (coerce b1 'single-float) vertices)
      (vector-push-extend (coerce a1 'single-float) vertices)
      (vector-push-extend (coerce x2 'single-float) vertices)
      (vector-push-extend (coerce y2 'single-float) vertices)
      (vector-push-extend (coerce z2 'single-float) vertices)
      (vector-push-extend (coerce r2 'single-float) vertices)
      (vector-push-extend (coerce g2 'single-float) vertices)
      (vector-push-extend (coerce b2 'single-float) vertices)
      (vector-push-extend (coerce a2 'single-float) vertices)

      (vector-push-extend index indices)
      (vector-push-extend (1+ index) indices))))

(defun random-line-cloud (&optional (n 100))
  (let ((lines (make-point-cloud)))
    (dotimes (i n)
      (add-line lines
                 :x1 (ju:random-between -0.25f0 0.25f0)
                 :y1 (ju:random-between -0.25f0 0.25f0)
                 :z1 (ju:random-between -0.25f0 0.25f0)
                 :r1 (ju:random-between 0.25f0 1.0f0)
                 :g1 (ju:random-between 0.0f0 1.0f0)
                 :b1 (ju:random-between 0.0f0 1.0f0)
                 :a1 (ju:random-between 0.5f0 1.0f0)
                 :x2 (ju:random-between -0.25f0 0.25f0)
                 :y2 (ju:random-between -0.25f0 0.25f0)
                 :z2 (ju:random-between -0.25f0 0.25f0)
                 :r2 (ju:random-between 0.25f0 1.0f0)
                 :g2 (ju:random-between 0.0f0 1.0f0)
                 :b2 (ju:random-between 0.0f0 1.0f0)
                 :a2 (ju:random-between 0.5f0 1.0f0)))
    lines))



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
  (let ((ls (make-line-segments)))
    (add-line ls
                    :x1 0.0 :y1 0.0 :z1 0.0
                    :x2 1.0 :y2 0.0 :z2 0.0)
    (add-line ls
                    :x1 1.0 :y1 0.0 :z1 0.0
                    :x2 1.0 :y2 1.0 :z2 0.0)
    (add-line ls
                     :x1 1.0 :y1 1.0 :z1 0.0
                     :x2 0.0 :y2 1.0 :z2 0.0)
    (add-line ls
                    :x1 0.0 :y1 1.0 :z1 0.0
                    :x2 0.0 :y2 0.0 :z2 0.0)
    ls))

;; (defun parametric-point-cloud ()
;;   (let* ((pc (newgl:make-point-cloud))
;;          (scale (/ 1 (* 2 pi)))
;;          (i-steps 180)
;;          (j-steps 180)
;;          (u-min (- pi))
;;          (v-min (- pi))
;;          (du (/ (* 2 pi) i-steps))
;;          (dv (/ (* 2 pi) j-steps)))
;;     (dotimes (i i-steps)
;;       (let ((uv (+ u-min (* i du))))
;;         (dotimes (j j-steps)
;;           (let* ((vv (+ v-min (* j dv)))
;;                  (xv uv)
;;                  (yv (* 3.0 (sin uv) (cos vv)))
;;                  (zv vv))
;;             (newgl:add-point pc :x xv :y yv :z zv)))))
;;     pc))
;; (newgl:viewer pc
;;               :xform
;;               (3d-matrices:m* (3d-matrices:mscaling (3d-vectors:vec3 scale scale scale))
;;                               (3d-matrices:mrotation (3d-vectors:vec3 1.0 0.0 0.0) (/ pi 3))
;;                               (3d-matrices:mrotation (3d-vectors:vec3 0.0 1.0 0.0) (/ pi 3)))))
