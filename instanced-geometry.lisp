;; instanced-geometry.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass instanced-geometry (geometry)
  ((instance-count :initform 0 :initarg :instance-count :accessor instance-count)
   (instance-vbo :initform 0)
   )
  (:documentation "Base class for all objects that can be rendered in a scene."))


(defmethod cleanup ((object geometry))
  (with-slots (instance-vbo instance-count) object
    ;; Do something
    (format t "Cleaning up instanced-geometry leaking ~a.~%" instance-vbo))
  (call-next-method))

(defmethod fill-buffers ((object geometry))
  (call-next-method)
  (with-slots (instance-vbo) object
    (setf instance-vbos (gl:gen-buffers 1))
    (multiple-value-bind (vertices indices) (vertex-buffers object)
      (let ((gl-vertices (to-gl-float-array vertices))
            (gl-indices (to-gl-array indices :unsigned-int)))

        (gl:bind-buffer :array-buffer (car vbos))
        (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
        (gl:free-gl-array gl-vertices)

        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
        (gl:free-gl-array gl-indices)
        (setf idx-count (length indices)))))
  ;; (with-slots (vbos ebos idx-count) object
  ;;   (when (not (null vbos))
  ;;     (error "vbos is being rebound!"))
  ;;   (when (not (null ebos))
  ;;     (error "ebos is being rebound!"))

  ;;   (setf vbos (gl:gen-buffers 1))
  ;;   (setf ebos (gl:gen-buffers 1))

  ;;   (multiple-value-bind (vertices indices) (vertex-buffers object)
  ;;     (let ((gl-vertices (to-gl-float-array vertices))
  ;;           (gl-indices (to-gl-array indices :unsigned-int)))

  ;;       (gl:bind-buffer :array-buffer (car vbos))
  ;;       (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
  ;;       (gl:free-gl-array gl-vertices)

  ;;       (gl:bind-buffer :element-array-buffer (car ebos))
  ;;       (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
  ;;       (gl:free-gl-array gl-indices)
  ;;       (setf idx-count (length indices)))))
  )

(defmethod render ((object geometry) view-xform)
  (declare (ignorable object view-xform))
  (call-next-method)
  (with-slots (instance-count primitive-type idx-count) object
    (gl:draw-elements-instanced primitive-type
                                (gl:make-null-gl-array :unsigned-int)
                                instance-count
                                :count idx-count)))



