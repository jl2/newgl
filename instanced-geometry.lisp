;; instanced-geometry.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass instanced-geometry (geometry)
  ((instance-count :initform 0 :initarg :instance-count :accessor instance-count)
   (instance-vbos :initform 0)
   )
  (:documentation "Base class for all objects that can be rendered in a scene."))


(defmethod cleanup ((object instanced-geometry))
  (with-slots (instance-vbos) object
      (when instance-vbos
        (gl:bind-buffer :array-buffer 0)
        (gl:delete-buffers instance-vbos)
        (setf instance-vbos nil)))
  (call-next-method))

(defgeneric alloc-and-fill-instance-data (object)
  (:documentation "Allocated and fill an array of instance data."))

(defmethod alloc-and-fill-instance-data ((object instanced-geometry))
  (allocate-gl-array :float 0))

(defmethod fill-buffers ((object instanced-geometry))
  (call-next-method)
  (with-slots (instance-vbos) object
    (setf instance-vbos (gl:gen-buffers 1))
    (multiple-value-bind (data stride) (alloc-and-fill-instance-data object)
      (gl:bind-buffer :array-buffer (car instance-vbos))
      (gl:buffer-data :array-buffer :static-draw data)
      (gl:free-gl-array data)
      )))

(defmethod bind-buffers ((object instanced-geometry))
  (call-next-method)
  (with-slots (instance-vbos) object
    (when (null instance-vbos)
      (error "vbos is null"))
    (gl:bind-buffer :array-buffer (car instance-vbos))
    (gl:bind-buffer :element-array-buffer (car ebos))))

(defmethod render ((object instanced-geometry))
  (call-next-method)
  (with-slots (instance-count primitive-type idx-count) object
    (gl:draw-elements-instanced primitive-type
                                (gl:make-null-gl-array :unsigned-int)
                                instance-count
                                :count idx-count)))



