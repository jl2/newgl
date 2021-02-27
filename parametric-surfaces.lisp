;; parametric-surfaces.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass parametric-surface (opengl-object)
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

   (emit-position :initform t :initarg :emit-position)
   (emit-normal :initform t :initarg :emit-normal)
   (emit-uv :initform nil :initarg :emit-uv)
   (emit-color :initform t :initarg :emit-color)
   (vertices :initform nil)
   (indices :initform nil)
  ))

(defgeneric f_u_v (surface uv vv))

(defgeneric df_u_v (surface uv vv &key du dv))

(defmethod df_u_v ((surface parametric-surface) uv vv &key (du 0.1) (dv 0.1))
  (let ((s1 (v- (f_u_v surface (+ uv du) vv)
                (f_u_v surface uv vv)))
        (s2 (v-
             (f_u_v surface uv (+ dv vv))
             (f_u_v surface uv vv))))
    (cond ((< (3d-vectors:vlength s1) 0.0001)
           (vec3 0.0 1.0 0.0))
          ((< (3d-vectors:vlength s2) 0.0001)
           (vec3 0.0 1.0 0.0))
          (t
           (nvunit (vc s1 s2))))))

(defun calculate-stride (obj)
  (with-slots (emit-position emit-normal emit-uv emit-color) obj
    (+ (if emit-position 3 0)
       (if emit-normal 3 0)
       (if emit-uv 2 0)
       (if emit-color 4 0))))

(defmethod initialize-buffers ((obj parametric-surface) &key)
  (with-slots (vertices indices
               color u-min v-min u-max v-max u-steps v-steps
               s-min s-max t-min t-max
               emit-position emit-normal emit-uv emit-color) obj
    (let* ((cur-vert-idx 0)
           (cur-idx-idx 0)

           (dst (/ (- s-max s-min) u-steps))
           (dtt (/ (- t-max t-min) v-steps))

           (du (/ (- u-max u-min) u-steps 1.0f0))
           (dv (/ (- v-max v-min) v-steps 1.0f0))
           (stride (calculate-stride obj)))

      (setf vertices (allocate-gl-array :float (* 2 3 u-steps v-steps stride)))
      (setf indices (allocate-gl-array :unsigned-int (*  2 3 u-steps v-steps)))

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
                                 (n0 (df_u_v obj uv vv))

                                 (pt1 (f_u_v obj (+ du uv) vv))
                                 (n1 (df_u_v obj (+ du uv) vv))

                                 (pt2 (f_u_v obj uv (+ vv dv)))
                                 (n2 (df_u_v obj uv (+ vv dv)))

                                 (pt3 (f_u_v obj (+ uv du) (+ vv dv)))
                                 (n3 (df_u_v obj (+ uv du) (+ vv dv))))
                            (next-point pt1 n1 (+ dst st) tt)
                            (next-point pt0 n0 st tt)
                            (next-point pt2 n2 st (+ tt dtt))

                            (next-point pt1 n1 (+ dst st) tt)
                            (next-point pt2 n2 st (+ tt dtt))
                            (next-point pt3 n3 (+ st dst) (+ tt dtt))
                            ))))
      (add-buffer obj (make-instance 'attribute-buffer
                                     :count (* 2 3 u-steps v-steps stride)
                                     :pointer vertices
                                     :stride nil
                                     :attributes (concatenate 'list
                                                              (when emit-position
                                                              '(("in_position" . :vec3)))
                                                              (when emit-normal
                                                                '(("in_normal" . :vec3)))
                                                              (when emit-color
                                                                '(("in_color" . :vec4)))
                                                              (when emit-uv
                                                                '(("in_uv" . :vec2))))
                                     :usage :static-draw
                                     :free nil))
      (add-buffer obj (make-instance 'index-buffer
                                     :count (*  2 3 u-steps v-steps)
                                     :pointer indices
                                     :stride nil
                                     :usage :static-draw
                                     :free nil)))))

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

(defmethod df_u_v ((surface sphere) uv vv &key (du 0.1) (dv 0.1))
  (declare (ignorable du dv))
  (nvunit (f_u_v surface uv vv)))

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

(defmethod df_u_v ((surface sombrero) uv vv &key (du 0.01) (dv 0.01))
  (let ((s2 (v- (f_u_v surface (+ uv du) vv)
                (f_u_v surface uv vv)))
        (s1 (v-
             (f_u_v surface uv (+ dv vv))
             (f_u_v surface uv vv))))
    (cond ((< (3d-vectors:vlength s1) 0.0001)
           (vec3 0.0 1.0 0.0))
          ((< (3d-vectors:vlength s2) 0.0001)
           (vec3 0.0 1.0 0.0))
          (t
           (nvunit (vc s1 s2))))))

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
