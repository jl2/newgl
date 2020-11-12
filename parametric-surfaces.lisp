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
   (v-max :initform pi :initarg :v-max)

   (s-min :initform 0.0 :initarg :s-min)
   (s-max :initform 1.0 :initarg :s-max)
   (t-min :initform 0.0 :initarg :t-min)
   (t-max :initform 1.0 :initarg :t-max)
  ))

(defgeneric f_u_v (surface uv vv))


(defmethod allocate-and-fill-buffers ((obj parametric-surface))
  (format t "In allocated-and-fill-buffers.~%")
  (with-slots (color u-min v-min u-max v-max u-steps v-steps
               s-min s-max t-min t-max) obj
    (let* ((desc (get-layout-descriptor obj))
           (vertices (allocate-gl-array :float (* 2 3 u-steps v-steps (layout-stride desc))))
           (indices (allocate-gl-array :unsigned-int (*  2 3 u-steps v-steps)))

           (cur-vert-idx 0)
           (cur-idx-idx 0)
           
           (dst (/ (- s-max s-min) u-steps))
           (dtt (/ (- t-max t-min) v-steps))

           (du (/ (- u-max u-min) u-steps 1.0f0))
           (dv (/ (- v-max v-min) v-steps 1.0f0)))
      (format t "desc: ~a~%" desc)
      (with-slots (emit-position emit-normal emit-uv emit-color) desc
        (labels ((next-point ( pos normal st tt)
                   (when emit-position
                     (gl-set vertices cur-vert-idx (vx pos) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vy pos) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vz pos) 'single-float)
                     (incf cur-vert-idx))
                   (when emit-normal
                     (gl-set vertices cur-vert-idx (vx normal) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vy normal) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vz normal) 'single-float)
                     (incf cur-vert-idx))
                   (when emit-uv
                     (format t "Emit uv ~a ~a~%" st tt)
                     (gl-set vertices cur-vert-idx st 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx tt 'single-float)
                     (incf cur-vert-idx))
                   (when emit-color
                     (gl-set vertices cur-vert-idx (vx color) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vy color) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vz color) 'single-float)
                     (incf cur-vert-idx)
                     (gl-set vertices cur-vert-idx (vw color) 'single-float)
                     (incf cur-vert-idx))
                   (when (or emit-color emit-uv emit-normal emit-position)
                     (gl-set indices cur-idx-idx cur-idx-idx 'fixnum)
                     (incf cur-idx-idx))))
          (loop for ui below u-steps
                for uv = (+ u-min (* ui du))
                for st = (+ s-min (* ui dst))
                do
                (loop for vi from 0 below v-steps
                      for vv = (+ v-min (* vi dv))
                      for tt = (+ t-min (* vi dtt))
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
