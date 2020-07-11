;; opengl-object.lisp
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (xform :initform (meye 4) :initarg :xform :type mat4)
   (shader-program :initarg :shader-program :accessor program))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defclass vertex-object (opengl-object)
  ((vertices :initarg :vertices)
   (indices :initarg :indices)
   (primitive-type :initarg :primitive-type :initform :triangles))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value)
  (:documentation "Set a uniform variable on object."))

(defgeneric set-uniforms (object)
  (:documentation "Assign uniform shader variables for this object."))

(defgeneric fill-buffers (object)
  (:documentation "Copy this objects data into OpenGL buffers.  Binding correct VAO is handled by before and after methods."))

(defgeneric reload-object (object)
  (:documentation "Destroy and reload object's buffers."))


(defmethod handle-key ((object opengl-object) window key scancode action mod-keys)
  (declare (ignorable object window key scancode action mod-keys))
  nil)

(defmethod handle-resize ((object opengl-object) window width height)
  (declare (ignorable window width height))
  (with-slots (xform) object
    (setf xform (3d-matrices:mscaling
                 (if (< width height )
                     (3d-vectors:vec3 (/ height width 1.0) 1.0 1.0)
                     (3d-vectors:vec3 (/ width height 1.0) 1.0 1.0))))
    (when *debug-stream* (format *debug-stream* "Transform: ~a~%" xform)))
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


(defun ensure-vao-bound (object)
  (with-slots (vao) object
    (when (= 0 vao)
      (setf vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)))

(defmethod update ((object opengl-object))
  ;; No animation!
  )

(defmethod build-shader-program :before ((object opengl-object))
  (ensure-vao-bound object))

(defmethod build-shader-program ((object opengl-object))
  (with-slots (shader-program) object
    (build-shader-program shader-program)))

(defmethod build-shader-program :after ((object opengl-object))
  (gl:bind-vertex-array 0))


(defmethod set-uniform ((obj opengl-object) name value)
  (with-slots (shader-program) obj
    (set-uniform shader-program name value)))


(defmethod set-uniforms ((object opengl-object))
  (with-slots (shader-program xform) object
    (set-uniform shader-program "transform" xform)))

(defmethod fill-buffers :before ((object opengl-object))
  (ensure-vao-bound object))

(defmethod fill-buffers ((object opengl-object)))

(defmethod fill-buffers ((object vertex-object))
  (with-slots (vbos ebos vertices indices) object
    (cond ((null vbos)
           (setf vbos (gl:gen-buffers 1))
           (setf ebos (gl:gen-buffers 1))
           (let ((gl-vertices (to-gl-float-array vertices))
                 (gl-indices (to-gl-array indices :unsigned-int)))

             (gl:bind-buffer :array-buffer (car vbos))
             (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car ebos))
             (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car vbos))
           (gl:bind-buffer :element-array-buffer (car ebos))))))

(defmethod fill-buffers :after ((object opengl-object))
  (gl:bind-vertex-array 0))


(defmethod reload-object ((object opengl-object))
  (cleanup object)
  (fill-buffers object)
  (build-shader-program object)
  (set-uniforms object))

(defmethod cleanup ((object opengl-object))
  (with-slots (vao vbos ebos) object
    (when (/= 0 vao)
      (when vbos
        (gl:delete-buffers vbos))
      (when ebos
        (gl:delete-buffers ebos))
      (gl:delete-vertex-arrays (list vao)))
    (setf vao 0)
    (setf vbos nil)))

(defmethod render :before ((object opengl-object) view-xform)
  (ensure-vao-bound object)
  (with-slots (vbos ebos indices shader-program xform) object
    (when (and vbos ebos)
      (gl:bind-buffer :array-buffer (car vbos))
      (gl:bind-buffer :element-array-buffer (car ebos))
      (use-shader-program shader-program)
      (set-uniform shader-program "transform" (m* view-xform xform)))))

(defmethod render ((object opengl-object) view-xform)
  (declare (ignorable view-xform)))

(defmethod render ((object vertex-object) view-xform)
  (declare (ignorable view-xform))
  (with-slots (indices primitive-type) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements primitive-type (gl:make-null-gl-array :unsigned-int) :count (length indices))))

(defmethod render :after ((object opengl-object) view-xform)
  (declare (ignorable view-xform))
  (gl:bind-vertex-array 0))

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
