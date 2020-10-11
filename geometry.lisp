;; geometry.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass geometry (opengl-object)
  ((vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (idx-count :initform 0)
   (primitive-type :initarg :primitive-type :initform :triangles))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod cleanup ((object geometry))
  (with-slots (vbos ebos) object
      (when vbos
        (gl:bind-buffer :array-buffer 0)
        (gl:delete-buffers vbos)
        (setf vbos nil))
      (when ebos
        (gl:bind-buffer :element-array-buffer 0)
        (gl:delete-buffers ebos)
        (setf ebos nil))
      (call-next-method)))

(defgeneric vertex-buffers (object)
  (:documentation "Generate vertices and indices for the object."))

(defmethod vertex-buffers ((object geometry))
  (values (make-array 0
                      :element-type 'single-float
                      :initial-contents '()
                      :adjustable t
                      :fill-pointer 0)
         (make-array 0
                      :element-type 'fixnum
                      :initial-contents '()
                      :adjustable t
                      :fill-pointer 0)))


(defmethod fill-buffers ((object geometry))
  (call-next-method)
  (with-slots (vbos ebos idx-count) object
    (when (not (null vbos))
      (error "vbos is being rebound!"))
    (when (not (null ebos))
      (error "ebos is being rebound!"))

    (setf vbos (gl:gen-buffers 1))
    (setf ebos (gl:gen-buffers 1))

    (multiple-value-bind (vertices indices) (vertex-buffers object)
      (let ((gl-vertices (to-gl-float-array vertices))
            (gl-indices (to-gl-array indices :unsigned-int)))

        (gl:bind-buffer :array-buffer (car vbos))
        (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
        (gl:free-gl-array gl-vertices)

        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
        (gl:free-gl-array gl-indices)
        (setf idx-count (length indices))))))

(defmethod bind-buffers ((object geometry))
  (call-next-method)
  (with-slots (vbos ebos) object
    (when (null vbos)
      (error "vbos is null"))
    (when (null ebos)
      (error "ebos is null"))
    (gl:bind-buffer :array-buffer (car vbos))
    (gl:bind-buffer :element-array-buffer (car ebos))))

(defmethod render ((object geometry) view-xform)
  (declare (ignorable object view-xform))
  (call-next-method)
  (with-slots (primitive-type idx-count) object
    (gl:draw-elements primitive-type (gl:make-null-gl-array :unsigned-int) :count idx-count)))



(defclass parametric-surface (geometry)
  ((color :initform (vec4 0.0 0.9 0.0 1.0) :initarg :color)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 16 :initarg :v-steps)
   (u-min :initform (- pi) :initarg :u-min)
   (v-min :initform (- pi) :initarg :v-min)
   (u-max :initform pi :initarg :u-max)
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
           (du (/ (- u-max u-min) u-steps))
           (dv (/ (- v-max v-min) v-steps)))
      (labels ((next-point ( pos normal uv vv)
                 (when (emit-position desc)
                   (vector-push-extend (coerce (vx pos) 'single-float) vertices)
                   (vector-push-extend (coerce (vy pos) 'single-float) vertices)
                   (vector-push-extend (coerce (vz pos) 'single-float) vertices))
                 (when (emit-normal desc)
                   (vector-push-extend (coerce (vx normal) 'single-float) vertices)
                   (vector-push-extend (coerce (vy normal) 'single-float) vertices)
                   (vector-push-extend (coerce (vz normal) 'single-float) vertices))
                 (when (emit-uv desc)
                   (vector-push-extend (coerce (f-map-val uv u-min u-max 0.0 1.0) 'single-float) vertices)
                   (vector-push-extend (coerce (f-map-val vv v-min v-max 0.0 1.0) 'single-float) vertices))
                 (when (emit-color desc)
                   (vector-push-extend (coerce (vx color) 'single-float) vertices)
                   (vector-push-extend (coerce (vy color) 'single-float) vertices)
                   (vector-push-extend (coerce (vz color) 'single-float) vertices)
                   (vector-push-extend (coerce (vw color) 'single-float) vertices))
                 (vector-push-extend index indices)
                 (incf index)))
        (loop for ui below (1+ u-steps)
              for uv = (+ u-min (* ui du))
              do
              (loop for vi from 0 below (1+ v-steps)
                    for vv = (+ v-min (* vi dv))
                    do
                    (let* ((pt0 (f_u_v obj uv vv))
                           (pt1 (f_u_v obj (+ du uv) vv))
                           (pt2 (f_u_v obj uv (+ vv dv)))
                           (pt3 (f_u_v obj (+ uv du) (+ vv dv)))
                           (normal (nvunit (vc (v- pt1 pt0)
                                                  (v- pt2 pt0)))))
                      (next-point pt1 normal (+ du uv) vv)
                      (next-point pt0 normal uv vv)
                      (next-point pt2 normal uv (+ vv dv))

                      (next-point pt1 normal (+ du uv) vv)
                      (next-point pt2 normal uv (+ vv dv))
                      (next-point pt3 normal (+ uv du) (+ vv dv))))))
      (values vertices indices))))

(defclass sphere (parametric-surface)
  ((radius :initform 1.0 :initarg :radius)
   (u-min :initform 0.0 :initarg :u-min)
   (v-min :initform 0.0 :initarg :v-min)
   (u-max :initform pi :initarg :u-max)
   (v-max :initform (* 2 pi) :initarg :v-max)))

(defmethod f_u_v ((sphere sphere) uv vv)
  (with-slots (radius) sphere
    (vec3 (* radius  (cos uv) (cos vv))
          (* radius (sin vv))
          (* radius (sin uv) (cos vv)))))

(defclass sin-surface (parametric-surface)
  ((height :initform 1.0 :initarg :height)
   (u-min :initform (* -1 pi) :initarg :u-min)
   (v-min :initform (* -1 pi) :initarg :v-min)
   (u-max :initform (* 1 pi) :initarg :u-max)
   (v-max :initform (* 1 pi) :initarg :v-max)))

(defmethod f_u_v ((surf sin-surface) uv vv)
  (with-slots (height) surf
    (vec3 uv
          (let ((r (+ (sqrt (+ (* uv uv)
                            (* vv vv)))
                      0.001)))
            (* height (/ (sin (* 4 r)) r)))
          vv)))
