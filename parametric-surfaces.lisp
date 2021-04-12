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
   (shaders :initform (newgl:plastic) :initarg :shaders)
  ))

(defmethod show-info ((object parametric-surface) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(u-min u-max
                                 v-min v-max
                                 nil
                                 u-steps
                                 v-steps
                                 nil
                                 s-min s-max
                                 t-min t-max
                                 nil
                                 emit-position
                                 emit-normal
                                 emit-uv))))

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
  (with-slots (emit-position emit-normal emit-uv) obj
    (+ (if emit-position 3 0)
       (if emit-normal 3 0)
       (if emit-uv 2 0))))

(defmethod initialize-buffers ((obj parametric-surface) &key)
  (with-slots (
               u-min v-min u-max v-max u-steps v-steps
               s-min s-max t-min t-max
               emit-position emit-normal emit-uv
               instance-count) obj

    (let* (
           (dst (/ (- s-max s-min) u-steps))
           (dtt (/ (- t-max t-min) v-steps))

           (du (/ (- u-max u-min) u-steps 1.0f0))
           (dv (/ (- v-max v-min) v-steps 1.0f0))
           (stride (calculate-stride obj))

           (vertices (allocate-gl-array :float (* 2 3 u-steps v-steps stride)))
           (indices (allocate-gl-array :unsigned-int (*  2 3 u-steps v-steps)))
           (cur-offset 0)
           (cur-idx-idx 0))
      (labels ((emit (pt norm uv)
                 (when emit-position
                   (setf cur-offset (fill-buffer pt vertices cur-offset)))
                 (when emit-normal
                   (setf cur-offset (fill-buffer norm vertices cur-offset)))
                 (when emit-uv
                   (setf cur-offset (fill-buffer uv vertices cur-offset)))
                 (when (or emit-uv emit-normal emit-position)
                   (gl-iset indices cur-idx-idx cur-idx-idx)
                   (incf cur-idx-idx))))
        (loop
            for ui below u-steps
            for uv = (+ u-min (* ui du))
            for st = (+ s-min (* ui dst))
            do
               (loop for vi from 0 below v-steps
                     for vv = (+ v-min (* vi dv))
                     for tt = (+ t-min (* vi dtt))
                     do
                        (let* ((pt0 (f_u_v obj uv vv))
                               (n0 (df_u_v obj uv vv))
                               (st0 (vec2 st tt))

                               (pt1 (f_u_v obj (+ du uv) vv))
                               (n1 (df_u_v obj (+ du uv) vv))
                               (st1 (vec2 (+ dst st) tt))

                               (pt2 (f_u_v obj uv (+ vv dv)))
                               (n2 (df_u_v obj uv (+ vv dv)))
                               (st2 (vec2  st (+ dtt tt)))

                               (pt3 (f_u_v obj (+ uv du) (+ vv dv)))
                               (n3 (df_u_v obj (+ uv du) (+ vv dv)))
                               (st3 (vec2 (+ dst st) (+ dtt tt))))

                          (emit pt0 n0 st0)
                          (emit pt2 n2 st2)
                          (emit pt1 n1 st1)

                          (emit pt1 n1 st1)
                          (emit pt2 n2 st2)
                          (emit pt3 n3 st3)
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
                                                            (when emit-uv
                                                              '(("in_uv" . :vec2))))
                                   :usage :static-draw
                                   :free t))
    (add-buffer obj (make-instance 'index-buffer
                                   :count (*  2 3 u-steps v-steps)
                                   :pointer indices
                                   :stride nil
                                   :usage :static-draw
                                   :free t))

      (let* ((inst-count 2000)
             (colors (loop for i below inst-count collecting (vec4-random 0.25 1.0)))
             (mats (loop for i below inst-count collecting
                                                (m* (mtranslation (vec3-random -2.0 2.0))
                                                    (mscaling (vec3 0.025 0.025 0.025))
                                                    (mrotation (vec3 1.0 0.0 0.0) (random (/ pi 2)))
                                                    (mrotation (vec3 0.0 1.0 0.0) (random (/ pi 2)))
                                                    (mrotation (vec3 0.0 0.0 1.0) (random (/ pi 2)))
                                                    ))))

        (add-buffer obj (make-instance 'instance-buffer
                                       :count inst-count
                                       :pointer (to-gl-array :float (* 16 inst-count) mats)
                                       :stride nil
                                       :usage :static-draw
                                       :free t))
        (add-buffer obj (make-instance 'instance-buffer
                                       :count inst-count
                                       :pointer (to-gl-array :float (* 4 inst-count) colors)
                                       :stride nil
                                       :attributes '(("in_color" . :vec4))
                                       :usage :static-draw
                                       :free t))
        (setf instance-count inst-count))
    )))

(defclass sphere (parametric-surface)
  ((radius :initform 1.0f0 :initarg :radius)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 32 :initarg :v-steps)
   (u-min :initform 0.0f0 :initarg :u-min)
   (u-max :initform (* 2 pi) :initarg :u-max)
   (v-min :initform 0.0f0 :initarg :v-min)
   (v-max :initform pi :initarg :v-max)
   (name :initform "sphere" :initarg :name)))

(defmethod show-info ((object sphere) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(radius))))

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

(defmethod show-info ((object sombrero) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(height))))

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
           (nvunit (vc s2 s1))))))

(defclass torus (parametric-surface)
  ((inner-radius :initform 0.125 :initarg :inner)
   (outer-radius :initform 0.5 :initarg :outer)
   (u-steps :initform 32 :initarg :u-steps)
   (v-steps :initform 64 :initarg :v-steps)

   (u-min :initform 0.0 :initarg :u-min)
   (u-max :initform (* 2 pi) :initarg :u-max)

   (v-min :initform 0 :initarg :v-min)
   (v-max :initform (* 2 pi) :initarg :v-max)))

(defmethod show-info ((object torus) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(inner-radius outer-radius))))

(defmethod f_u_v ((surf torus) θ φ)
  (with-slots (inner-radius outer-radius) surf
    (let ((R+rcosθ (+ outer-radius (* inner-radius (cos θ)))))
      (vec3 (* R+rcosθ (cos φ))
            (* R+rcosθ (sin φ))
            (* inner-radius (sin θ))))))
