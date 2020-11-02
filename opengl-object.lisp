;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (xform :initform (meye 4) :initarg :xform :type mat4)
   (shader-program :initarg :shader-program :accessor program))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value)
  (:documentation "Set a uniform variable on object."))

(defgeneric assign-uniforms (object &optional view-xform)
  (:documentation "Assign uniform shader variables for this object."))

(defgeneric fill-buffers (object)
  (:documentation "Copy this objects data into OpenGL buffers.  Binding correct VAO is handled by before and after methods."))

(defgeneric bind-buffers (object))

(defgeneric reload-object (object)
  (:documentation "Destroy and reload object's buffers."))

(defclass gl-buffer ()
  ((size :initform 0)
   (type :initform :float)))

(defun allocate-gl-array (type count)
  (gl:alloc-gl-array type count))

(defun gl-set (array idx value type)
  (setf (gl:glaref array idx)
        (coerce value type)))

(defun gl-get (array idx)
  (gl:glaref array idx))

(defun to-gl-float-array (sequence)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3))
           (type sequence sequence))
  (let* (
         (count (length sequence))
         (arr (make-array (length sequence) :initial-contents sequence :element-type 'single-float))
         (gl-array (gl:alloc-gl-array :float count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) 'single-float)))
    gl-array))

(defun to-gl-array (arr type)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3))
           (type (vector) arr))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (aref arr i)))
    gl-array))


(defmethod handle-key ((object opengl-object) window key scancode action mod-keys)
  (declare (ignorable object window key scancode action mod-keys))
  nil)

(defmethod handle-resize ((object opengl-object) window width height)
  (declare (ignorable window width height))
  nil)

(defmethod handle-click ((object opengl-object) window click-info)
  (declare (ignorable object window click-info))
  nil)

(defmethod handle-scroll ((object opengl-object) window cpos x-scroll y-scroll)
  (declare (ignorable object window cpos x-scroll y-scroll))
  nil)

(defmethod update ((object opengl-object) elapsed-seconds )
  (declare (ignorable object elapsed-seconds)))


(defmethod build-shader-program ((object opengl-object))
  (with-slots (shader-program) object
    (build-shader-program shader-program)))


(defmethod set-uniform ((obj opengl-object) name value)
  (with-slots (shader-program) obj
    (set-uniform shader-program name value)))

(defmethod assign-uniforms ((object opengl-object) &optional (view-xform (meye 4)))
  (with-slots (shader-program xform) object
    (let ((mv (m* view-xform xform  )))
      (set-uniform shader-program "transform" mv)
      (set-uniform shader-program "normalTransform" (mtranspose (minv mv))))
    (assign-uniforms shader-program view-xform)))

(defmethod fill-buffers ((object opengl-object))
  (with-slots (shader-program vao) object
    (when (/= 0 vao)
      (error "fill-buffers called on object where vao != 0"))
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)))

(defmethod fill-buffers :after ((object opengl-object))
  (with-slots (shader-program vao) object
    (fill-buffers shader-program)))

(defmethod reload-object ((object opengl-object))
  (cleanup object)
  (build-shader-program object)
  (fill-buffers object))

(defmethod cleanup ((object opengl-object))
  (with-slots (vao vbos ebos shader-program) object
    (when (/= 0 vao)
      (when shader-program
        (cleanup shader-program))
      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind-buffers ((object opengl-object))
  (with-slots (shader-program vao) object
    (gl:bind-vertex-array vao)))

(defmethod bind-buffers :after ((object opengl-object))
  (with-slots (shader-program) object
    (bind-buffers shader-program)))

(defmethod render ((object opengl-object) view-xform)
  (with-slots (vao shader-program xform) object
    (if (zerop vao)
        (fill-buffers object)
        (bind-buffers object))
    (assign-uniforms object view-xform)
    (use-shader-program shader-program)))


