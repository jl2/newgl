;; vertex-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass vertex-object (opengl-object)
  ((vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (vertices :initarg :vertices)
   (indices :initarg :indices)
   (primitive-type :initarg :primitive-type :initform :triangles))
  (:documentation "Base class for all objects that can be rendered in a scene."))


(defmethod cleanup ((object vertex-object))
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

(defmethod fill-buffers ((object vertex-object))
  (call-next-method)
  (with-slots (vbos ebos vertices indices) object
    (when (not (null vbos))
      (error "vbos is being rebound!"))
    (when (not (null ebos))
      (error "ebos is being rebound!"))

    (setf vbos (gl:gen-buffers 1))
    (setf ebos (gl:gen-buffers 1))

    (let ((gl-vertices (to-gl-float-array vertices))
          (gl-indices (to-gl-array indices :unsigned-int)))

      (gl:bind-buffer :array-buffer (car vbos))
      (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
      (gl:free-gl-array gl-vertices)

      (gl:bind-buffer :element-array-buffer (car ebos))
      (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
      (gl:free-gl-array gl-indices))))

(defmethod bind-buffers ((object vertex-object))
  (call-next-method)
  (with-slots (vbos ebos) object
    (when (null vbos)
      (error "vbos is null"))
    (when (null ebos)
      (error "ebos is null"))

    (gl:bind-buffer :array-buffer (car vbos))
    (gl:bind-buffer :element-array-buffer (car ebos))))

(defmethod render ((object vertex-object) view-xform)
  (call-next-method)
  (with-slots (indices primitive-type) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements primitive-type (gl:make-null-gl-array :unsigned-int) :count (length indices))))

