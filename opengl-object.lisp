;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (xform :initform (meye 4) :initarg :xform :type mat4)
   (aspect-ratio :initform (vec3 1.0 1.0 1.0) :type vec3)
   (shader-program :initarg :shader-program :accessor program))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value)
  (:documentation "Set a uniform variable on object."))

(defgeneric set-uniforms (object)
  (:documentation "Assign uniform shader variables for this object."))

(defgeneric fill-buffers (object)
  (:documentation "Copy this objects data into OpenGL buffers.  Binding correct VAO is handled by before and after methods."))

(defgeneric bind-buffers (object))

(defgeneric reload-object (object)
  (:documentation "Destroy and reload object's buffers."))


(defun to-gl-float-array (arr)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3))
           (type vector arr))
  (let* ((count (length arr))
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
  (with-slots (aspect-ratio) object
    (setf aspect-ratio
          (if (< width height )
              (3d-vectors:vec3 (/ height width 1.0) 1.0 1.0)
              (3d-vectors:vec3 (/ width height 1.0) 1.0 1.0))))
  (set-uniforms object))

(defmethod handle-click ((object opengl-object) window click-info)
  (declare (ignorable object window click-info))
  nil)

(defmethod handle-scroll ((object opengl-object) window cpos x-scroll y-scroll)
  (declare (ignorable object window cpos x-scroll y-scroll))
  nil)

(defmethod handle-drag ((object opengl-object) window first-click-info current-pos)
  (declare (ignorable object window first-click-info current-pos))
  nil)


(defmethod update ((object opengl-object) elapsed-seconds)
  )

(defmethod build-shader-program ((object opengl-object))
  (with-slots (shader-program) object
    (build-shader-program shader-program)))

(defmethod set-uniform ((obj opengl-object) name value)
  (with-slots (shader-program) obj
    (set-uniform shader-program name value)))

(defmethod set-uniforms ((object opengl-object))
  ;; (with-slots (shader-program xform aspect-ratio) object
  ;;   (set-uniform shader-program "transform" (m* xform (3d-matrices:mscaling aspect-ratio))))
  )

(defmethod fill-buffers ((object opengl-object))
  (with-slots (vao) object
    (when (/= 0 vao)
      (error "fill-buffers called on object where vao != 0"))
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)))

(defmethod reload-object ((object opengl-object))
  (cleanup object)
  (fill-buffers object)
  (build-shader-program object)
  (set-uniforms object))

(defmethod cleanup ((object opengl-object))
  (with-slots (vao vbos ebos shader-program) object
    (when (/= 0 vao)
      (when shader-program
        (cleanup shader-program))
      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind-buffers ((object opengl-object))
  (with-slots (vao) object
    (gl:bind-vertex-array vao)))


(defmethod render ((object opengl-object) view-xform)
  (with-slots (shader-program xform aspect-ratio) object
    (bind-buffers object)
    (let ((old-xform xform))
      (set-uniform shader-program "transform" (m*  xform (3d-matrices:mscaling aspect-ratio) view-xform ))
      (use-shader-program shader-program)
      (setf xform old-xform))))


