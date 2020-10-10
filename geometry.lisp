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


(defclass sphere (geometry)
  ((radius :initform 1.0 :initarg :radius)
   (u-steps :initform 16 :initarg :u-steps)
   (v-steps :initform 16 :initarg :v-steps)
   (u-min :initform (- pi) :initarg :u-min)
   (v-min :initform (- pi) :initarg :u-min)
   (u-max :initform pi :initarg :u-min)
   (v-max :initform pi :initarg :u-min)))

(defmethod vertex-buffers ((sphere sphere))
  (with-slots (radius u-min v-min u-max v-max u-steps v-steps) sphere
  (let* ((vertices (make-array (* u-steps v-steps (+ 3 3 4))
                               :element-type 'single-float
                               :initial-element 0.0f0
                               :adjustable t
                               :fill-pointer 0))
         (indices (make-array (* u-steps v-steps)
                              :element-type 'fixnum
                              :initial-element 0
                              :adjustable t
                              :fill-pointer 0))
         (index 0)
         (u-min (- pi))
         (v-min (- pi))
         (du (/ (- u-max u-min) u-steps))
         (dv (/ (- v-max v-min) v-steps)))
      (labels ((next-point ( x y z red green blue alpha)
                        (vector-push-extend (coerce x 'single-float) vertices)
                        (vector-push-extend (coerce y 'single-float) vertices)
                        (vector-push-extend (coerce z 'single-float) vertices)
                        (vector-push-extend (coerce x 'single-float) vertices)
                        (vector-push-extend (coerce y 'single-float) vertices)
                        (vector-push-extend (coerce z 'single-float) vertices)
                        (vector-push-extend (coerce red 'single-float) vertices)
                        (vector-push-extend (coerce green 'single-float) vertices)
                        (vector-push-extend (coerce blue 'single-float) vertices)
                        (vector-push-extend (coerce alpha 'single-float) vertices)
                        (vector-push-extend index indices))
               (fx (uv vv)
                 (declare (ignorable uv vv))
                 (* radius (cos uv) (cos vv)))
               (fy (uv vv)
                 (declare (ignorable uv vv))
                 (* radius (sin uv) (cos vv)))
               (fz (uv vv)
                 (declare (ignorable uv vv))
                 (* radius (sin vv)))
               (tm-add-point (uv vv)
                 (let ((xv (fx uv vv))
                       (yv (fy uv vv))
                       (zv (fz uv vv)))
                   (next-point xv yv zv
                               0.0 0.8 0.0 1.0)
                   (incf index))))
        (loop for ui below u-steps
              for uv = (+ u-min (* ui du))
              do
              (loop for vi from 0 below v-steps
                    for vv = (+ v-min (* vi dv))
                    do
                    (tm-add-point (+ uv du) vv)
                    (tm-add-point uv vv)
                    (tm-add-point uv (+ vv dv))

                    (tm-add-point (+ uv du) (+ vv dv))
                    (tm-add-point (+ uv du) vv)
                    (tm-add-point uv (+ vv dv))
                    )))
      (values vertices indices))))
