;; psurf.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass psurf (opengl-object)
  ((primitive-type :initform :triangles)
   (min-corner :initform (vec2 (- pi) (- pi)) :initarg :min-corner)
   (max-corner :initform (vec2 pi pi) :initarg :max-corner)
   (steps :initform (vec2 32.0 32.0) :initarg :steps)))

(defclass psurf-torus (psurf)
  ((shaders :initform (list (shader-from-file "psurf-torus-vertex.glsl")
                            (shader-from-file "point-fragment.glsl")))
   ))
(defclass psurf-quad (psurf)
  ((shaders :initform (list (shader-from-file "psurf-quad-vertex.glsl")
                            (shader-from-file "point-fragment.glsl")))
   ))

(defmethod show-info ((object psurf) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(min-corner max-corner steps))))

(defmethod initialize-buffers ((obj psurf) &key)
  (with-slots (min-corner max-corner steps instance-count) obj
    (let* (
           (step (v/ (v- max-corner min-corner) steps))

           (stride 2)
           (s-steps (ceiling (vx steps)))
           (t-steps (ceiling (vy steps)))
           (vertices (allocate-gl-array :float (* 2 3 stride s-steps t-steps)))
           (indices (allocate-gl-array :unsigned-int (* 2 3 s-steps t-steps )))
           (cur-offset 0)
           (cur-idx-idx 0))
      ;; (format t "steps: ~a s-steps ~a t-steps ~a step ~a~%"
      ;;         steps
      ;;         s-steps
      ;;         t-steps
      ;;         step)
      (labels ((emit (uv)
                 (setf cur-offset (fill-buffer uv vertices cur-offset))
                 (gl-iset indices cur-idx-idx cur-idx-idx)
                 (incf cur-idx-idx)))
        (loop
            for ui below s-steps
            do
               (loop
                 for vi from 0 below t-steps
                 do
                    ;; (format t "ui: ~a vi: ~a " ui vi)
                    (let* ((st0 (v+ min-corner (v* step (vec2 ui vi))))
                           (st1 (v+ min-corner (v* step (vec2 (1+ ui) vi))))
                           (st2 (v+ min-corner (v* step (vec2 ui (1+ vi)))))
                           (st3 (v+ min-corner (v* step (vec2 (1+ ui) (1+ vi))))))

                          (emit st0)
                          (emit st2)
                          (emit st1)

                          (emit st1)
                          (emit st2)
                          (emit st3)))))
    (add-buffer obj (make-instance 'attribute-buffer
                                   :count (* 2 3 s-steps t-steps stride)
                                   :pointer vertices
                                   :stride nil
                                   :attributes '(("in_uv" . :vec2))
                                   :usage :static-draw
                                   :free t))
    (add-buffer obj (make-instance 'index-buffer
                                   :count (*  2 3 s-steps t-steps)
                                   :pointer indices
                                   :stride nil
                                   :usage :static-draw
                                   :free t))

      (let* ((inst-count 1)
             (colors (list (vec4 1.0 0.0 0.0 1.0)))
             (mats (list (meye 4)))
             ;;(in-radi (list (vec2 0.25 1.25)))
             )


        ;; (add-buffer obj (make-instance 'instance-buffer
        ;;                                :count inst-count
        ;;                                :pointer (to-gl-array :float (* 2 inst-count) in-radi)
        ;;                                :attributes '(("in_radi" . :vec2))
        ;;                                :free t))
        (add-buffer obj (make-instance 'instance-buffer
                                       :count inst-count
                                       :pointer (to-gl-array :float (* 16 inst-count) mats)
                                       :free t
                                       :attributes '(("obj_transform" . :mat4))))
        (add-buffer obj (make-instance 'instance-buffer
                                       :count inst-count
                                       :pointer (to-gl-array :float (* 4 inst-count) colors)
                                       :attributes '(("in_color" . :vec4))
                                       :free t))
        (setf instance-count inst-count))
      )))

;; (defclass sphere (parametric-surface)
;;   ((radius :initform 1.0f0 :initarg :radius)
;;    (u-steps :initform 16 :initarg :u-steps)
;;    (v-steps :initform 32 :initarg :v-steps)
;;    (u-min :initform 0.0f0 :initarg :u-min)
;;    (u-max :initform (* 2 pi) :initarg :u-max)
;;    (v-min :initform 0.0f0 :initarg :v-min)
;;    (v-max :initform pi :initarg :v-max)
;;    (name :initform "sphere" :initarg :name)))

;; (defmethod show-info ((object sphere) &key (indent 0))
;;   (call-next-method)
;;   (let ((this-ws (indent-whitespace (+ 1 indent))))
;;     (show-slots this-ws object '(radius))))

;; (defmethod f_u_v ((sphere sphere) uv vv)
;;   (with-slots (radius) sphere
;;     (vec3 (* radius (cos uv) (sin vv))
;;           (* radius (cos vv))
;;           (* radius (sin uv) (sin vv)))))

;; (defmethod df_u_v ((surface sphere) uv vv &key (du 0.1) (dv 0.1))
;;   (declare (ignorable du dv))
;;   (nvunit (f_u_v surface uv vv)))

;; (defclass sombrero (parametric-surface)
;;   ((height :initform 1.0 :initarg :height)
;;    (u-steps :initform 32 :initarg :u-steps)
;;    (v-steps :initform 32 :initarg :v-steps)
;;    (u-min :initform -10.0 :initarg :u-min)
;;    (u-max :initform 10.0 :initarg :u-max)

;;    (v-min :initform -10.0 :initarg :v-min)
;;    (v-max :initform 10.0 :initarg :v-max)))

;; (defmethod show-info ((object sombrero) &key (indent 0))
;;   (call-next-method)
;;   (let ((this-ws (indent-whitespace (+ 1 indent))))
;;     (show-slots this-ws object '(height))))

;; (defmethod f_u_v ((surf sombrero) uv vv)
;;   (with-slots (height) surf
;;     (vec3 uv
;;           (let ((r (+ (sqrt (+
;;                              (* uv uv)
;;                              (* vv vv)))
;;                       0.001f0)))
;;             (* height (/ (sin (* 4 r)) r)))
;;           vv)))

;; (defmethod df_u_v ((surface sombrero) uv vv &key (du 0.01) (dv 0.01))
;;   (let ((s2 (v- (f_u_v surface (+ uv du) vv)
;;                 (f_u_v surface uv vv)))
;;         (s1 (v-
;;              (f_u_v surface uv (+ dv vv))
;;              (f_u_v surface uv vv))))
;;     (cond ((< (3d-vectors:vlength s1) 0.0001)
;;            (vec3 0.0 1.0 0.0))
;;           ((< (3d-vectors:vlength s2) 0.0001)
;;            (vec3 0.0 1.0 0.0))
;;           (t
;;            (nvunit (vc s2 s1))))))

;; (defclass torus (parametric-surface)
;;   ((inner-radius :initform 0.125 :initarg :inner)
;;    (outer-radius :initform 0.5 :initarg :outer)
;;    (u-steps :initform 32 :initarg :u-steps)
;;    (v-steps :initform 64 :initarg :v-steps)

;;    (u-min :initform 0.0 :initarg :u-min)
;;    (u-max :initform (* 2 pi) :initarg :u-max)

;;    (v-min :initform 0 :initarg :v-min)
;;    (v-max :initform (* 2 pi) :initarg :v-max)))

;; (defmethod show-info ((object torus) &key (indent 0))
;;   (call-next-method)
;;   (let ((this-ws (indent-whitespace (+ 1 indent))))
;;     (show-slots this-ws object '(inner-radius outer-radius))))

;; (defmethod f_u_v ((surf torus) θ φ)
;;   (with-slots (inner-radius outer-radius) surf
;;     (let ((R+rcosθ (+ outer-radius (* inner-radius (cos θ)))))
;;       (vec3 (* R+rcosθ (cos φ))
;;             (* R+rcosθ (sin φ))
;;             (* inner-radius (sin θ))))))


;; (defun make-parametric-with-instance-data (type instance-count colors matrices &rest params
;;                                            &key
;;                                              u-min
;;                                              u-max
;;                                              v-min
;;                                              v-max
;;                                              u-steps
;;                                              v-steps
;;                                            &allow-other-keys)
;;   (declare (ignorable u-min u-max v-min v-max u-steps v-steps))
;;   (apply #'make-instance type :instance-count instance-count :params))
