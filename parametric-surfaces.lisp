;; parametric-surfaces.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass parametric-surface (geometry)
  ((color :initform (vec4 0.0 0.9 0.0 1.0) :initarg :color)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 16 :initarg :v-steps)
   (u-min :initform (- pi) :initarg :u-min)
   (u-max :initform pi :initarg :u-max)

   (v-min :initform (- pi) :initarg :v-min)
   (v-max :initform pi :initarg :v-max)))

(defgeneric f_u_v (surface uv vv))

(defmethod vertex-buffers ((obj parametric-surface))
  (with-slots (shader-program color u-min v-min u-max v-max u-steps v-steps) obj
    (let* ((desc (get-layout-descriptor shader-program))
           (vertices (make-array (* u-steps v-steps (layout-stride desc))
                                 :element-type 'single-float
                                 :initial-element 0.0f0
                                 :adjustable t
                                 :fill-pointer 0))
           (indices (make-array (* u-steps v-steps 2)
                                :element-type 'fixnum
                                :initial-element 0
                                :adjustable t
                                :fill-pointer 0))
           (index 0)
           (dst (/ 1.0 u-steps))
           (dtt (/ 1.0 v-steps))

           (du (/ (- u-max u-min) u-steps 1.0f0))
           (dv (/ (- v-max v-min) v-steps 1.0f0)))
      (with-slots (emit-position emit-normal emit-uv emit-color) desc
        (labels ((next-point ( pos normal st tt)
                   (when emit-position
                     (vector-push-extend (coerce (vx pos) 'single-float) vertices)
                     (vector-push-extend (coerce (vy pos) 'single-float) vertices)
                     (vector-push-extend (coerce (vz pos) 'single-float) vertices))
                   (when emit-normal
                     (vector-push-extend (coerce (vx normal) 'single-float) vertices)
                     (vector-push-extend (coerce (vy normal) 'single-float) vertices)
                     (vector-push-extend (coerce (vz normal) 'single-float) vertices))
                   (when emit-uv
                     (vector-push-extend (coerce st 'single-float) vertices)
                     (vector-push-extend (coerce tt 'single-float) vertices))
                   (when emit-color
                     (vector-push-extend (coerce (vx color) 'single-float) vertices)
                     (vector-push-extend (coerce (vy color) 'single-float) vertices)
                     (vector-push-extend (coerce (vz color) 'single-float) vertices)
                     (vector-push-extend (coerce (vw color) 'single-float) vertices))
                   (vector-push-extend index indices)
                   (incf index)))
          (loop for ui below u-steps
                for uv = (+ u-min (* ui du))
                for st = (* ui dst)
                do
                (loop for vi from 0 below v-steps
                      for vv = (+ v-min (* vi dv))
                      for tt = (* vi dtt)
                      do
                    (let* ((pt0 (f_u_v obj uv vv))
                           (pt1 (f_u_v obj (+ du uv) vv))
                           (pt2 (f_u_v obj uv (+ vv dv)))
                           (pt3 (f_u_v obj (+ uv du) (+ vv dv)))
                           (normal (nvunit (vc
                                            (v- pt1 pt3)
                                            (v- pt2 pt3)))))
                        (next-point pt1 normal (+ dst st) tt)
                        (next-point pt0 normal st tt)
                        (next-point pt2 normal st (+ tt dtt))

                        (next-point pt1 normal (+ dst st) tt)
                        (next-point pt2 normal st (+ tt dtt))
                        (next-point pt3 normal (+ st dst) (+ tt dtt))
                        )))))
      (values vertices indices))))

(defclass sphere (parametric-surface)
  ((radius :initform 1.0f0 :initarg :radius)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 32 :initarg :v-steps)
   (u-min :initform 0.0f0 :initarg :u-min)
   (u-max :initform (* 2 pi) :initarg :u-max)
   (v-min :initform 0.0f0 :initarg :v-min)
   (v-max :initform pi :initarg :v-max)))

(defmethod f_u_v ((sphere sphere) uv vv)
  (with-slots (radius) sphere
    (vec3 (* radius (cos uv) (sin vv))
          (* radius (cos vv))
          (* radius (sin uv) (sin vv)))))

(defclass sombrero (parametric-surface)
  ((height :initform 1.0 :initarg :height)
   (u-steps :initform 32 :initarg :u-steps)
   (v-steps :initform 32 :initarg :v-steps)
   (u-min :initform -10.0 :initarg :u-min)
   (u-max :initform 10.0 :initarg :u-max)

   (v-min :initform -10.0 :initarg :v-min)
   (v-max :initform 10.0 :initarg :v-max)))

(defmethod f_u_v ((surf sombrero) uv vv)
  (with-slots (height) surf
    (vec3 uv
          (let ((r (+ (sqrt (+
                             (* uv uv)
                             (* vv vv)))
                      0.001f0)))
            (* height (/ (sin (* 4 r)) r)))
          vv)))

(defclass torus (parametric-surface)
  ((inner-radius :initform 0.25 :initarg :inner)
   (outer-radius :initform 1.0 :initarg :outer)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 16 :initarg :v-steps)

   (u-min :initform 0.0 :initarg :u-min)
   (u-max :initform (* 2 pi) :initarg :u-max)

   (v-min :initform 0 :initarg :v-min)
   (v-max :initform (* 2 pi) :initarg :v-max)))

(defmethod f_u_v ((surf torus) θ φ)
  (with-slots (inner-radius outer-radius) surf
    (let ((R+rcosθ (+ outer-radius (* inner-radius (cos θ)))))
      (vec3 (* R+rcosθ (cos φ))
            (* R+rcosθ (sin φ))
            (* inner-radius (sin θ))))))
