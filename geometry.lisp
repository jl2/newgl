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
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements primitive-type (gl:make-null-gl-array :unsigned-int) :count idx-count)))

