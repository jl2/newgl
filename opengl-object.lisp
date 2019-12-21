;;;; opengl-object.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (shader-program :initarg :program))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defclass vertex-object (opengl-object)
  ((vertices :initarg :vertices)
   (indices :initarg :indices))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric render (object)
  (:documentation "Make OpenGL API calls to render the object.  Binding correct VAO is handled by before and after methods."))

(defgeneric rebuild-shaders (object)
  (:documentation "Rebuild this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric update (object)
  (:documentation "Called on an object *before* rendering to update for the next animation frame."))

(defgeneric fill-buffers (object)
  (:documentation "Copy this objects data into OpenGL buffers.  Binding correct VAO is handled by before and after methods."))

(defgeneric handle-key (object window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (object window click-info)
  (:documentation "Handle mouse move."))

(defgeneric handle-scroll (object window cpos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-drag (object window first-click-info current-pos)
  (:documentation "Handle mouse drag."))

(defgeneric handle-resize (object window width height)
  (:documentation "Handle window resize."))

(defgeneric reload-object (object)
  (:documentation "Destroy and reload object's buffers."))

(defmethod handle-key ((object opengl-object) window key scancode action mod-keys)
  nil)

(defmethod handle-resize ((object opengl-object) window width height)
  nil)

(defmethod handle-click ((object opengl-object) window click-info)
  nil)

(defmethod handle-scroll ((object opengl-object) window cpos x-scroll y-scroll)
  nil)

(defmethod handle-drag ((object opengl-object) window first-click-info current-pos)
  nil)

(defun ensure-vao-bound (object)
  (with-slots (vao) object
    (when (= 0 vao)
      (setf vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)))

(defmethod update ((object opengl-object))
  ;; No animation!
  )

(defmethod rebuild-shaders :before ((object opengl-object))
  (ensure-vao-bound object))

(defmethod newgl:rebuild-shaders ((object opengl-object))
  (with-slots (shader-program) object
    (build-shader-program shader-program)))

(defmethod rebuild-shaders ((object opengl-object)))

(defmethod rebuild-shaders :after ((object opengl-object))
  (gl:bind-vertex-array 0))



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


(defmethod set-uniforms ((object opengl-object))
  ;; (format t "WARNING: opengl-object set-uniforms~%")
  )


(defmethod reload-object ((object opengl-object))
  (cleanup object)
  (fill-buffers object))

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

(defmethod render :before ((object opengl-object))
  (ensure-vao-bound object)
  (with-slots (vbos ebos indices shader-program) object
    (when (and vbos ebos)
      (gl:bind-buffer :array-buffer (car vbos))
      (use-shader-program shader-program)
      (gl:bind-buffer :element-array-buffer (car ebos))
      (set-uniforms object))))

(defmethod render ((object opengl-object)))

(defmethod render ((object vertex-object))
  (with-slots (indices) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))

(defmethod render :after ((object opengl-object))
  (gl:bind-vertex-array 0))


(defun to-gl-float-array (arr)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3))
           (type (vector single-float) arr))
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

