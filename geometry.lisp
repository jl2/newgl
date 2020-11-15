;; geometry.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass geometry (opengl-object)
  ((vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (primitive-type :initarg :primitive-type :initform :triangles)
   (idx-count :initform 0))
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

(defgeneric allocate-and-fill-buffers (object)
  (:documentation "Generate vertices and indices for the object."))

(defmethod allocate-and-fill-buffers ((object geometry))
  (values (allocate-gl-array :float 0)
          (allocate-gl-array :unsigned-int 0)))

(defmethod fill-buffers ((object geometry))
  (call-next-method)
  (with-slots (vbos ebos idx-count) object
    (when (not (null vbos))
      (error "vbos is being rebound!"))
    (when (not (null ebos))
      (error "ebos is being rebound!"))

    (setf vbos (gl:gen-buffers 1))
    (setf ebos (gl:gen-buffers 1))

    (multiple-value-bind (vertices indices) (allocate-and-fill-buffers object)
      (gl:bind-buffer :array-buffer (car vbos))
      (gl:buffer-data :array-buffer :static-draw vertices)
      (gl:free-gl-array vertices)

      (gl:bind-buffer :element-array-buffer (car ebos))
      (gl:buffer-data :element-array-buffer :static-draw indices)
      (setf idx-count (gl::gl-array-size indices))
      (gl:free-gl-array indices))))

(defmethod bind-buffers ((object geometry))
  (call-next-method)
  (with-slots (vbos ebos) object
    (when (null vbos)
      (error "vbos is null"))
    (when (null ebos)
      (error "ebos is null"))
    (gl:bind-buffer :array-buffer (car vbos))
    (gl:bind-buffer :element-array-buffer (car ebos))))

(defmethod render ((object geometry))
  (declare (ignorable object))
  (call-next-method)
  (with-slots (primitive-type idx-count) object
    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count idx-count)))



