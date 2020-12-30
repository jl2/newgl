;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (xform :initform (meye 4) :initarg :xform :type mat4)
   (shaders :initarg :shaders :initform nil :type (or null list))
   (textures :initarg :textures :initform nil :type (or null list))
   (buffers)
   (program :initform 0))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod show-info ((object opengl-object))
  (dolist (slot '(vao xform shaders textures program))
    (format t "  ~a: ~a~%" slot (slot-value object slot)))
  (with-slots (program) object
    (let ((shaders (gl:get-attached-shaders program)))
      (format t "Shaders: ~a~%" shaders)
      (dolist (shader shaders)
        (dolist (attrib '(:shader-type
                          :delete-status :compile-status :info-log-length :shader-source-length))
        (Format t "~a ~a: ~a~%" shader attrib (gl:get-shader shader attrib)))))))
  
(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value)
  (:documentation "Set a uniform variable on object."))

(defgeneric fill-buffers (object)
  (:documentation "Copy this objects data into OpenGL buffers.  Binding correct VAO is handled by before and after methods."))

(defgeneric bind-buffers (object))


(defclass gl-buffer ()
  ((size :initform 0 :initarg :size)
   (type :initform :float)))

(declaim (inline allocate-gl-array free-gl-array gl-set gl-get to-gl-float-array to-gl-array))
(defun allocate-gl-array (type count)
  (gl:alloc-gl-array type count))

(defun free-gl-array (array)
  (gl:free-gl-array array))

(defun gl-set (array idx value type)
  (setf (gl:glaref array idx)
        (coerce value type)))

(defun gl-get (array idx)
  (gl:glaref array idx))

(defun to-gl-array (gl-type arr)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3))
           (type (vector (or single-float fixnum)) arr))
  (let* ((count (length arr))
         (cl-type (assoc-value '((:float . 'single-float)
                                 (:unsigned-int . 'fixnum))
                               gl-type))
         (gl-array (allocate-gl-array gl-type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) cl-type)))
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

(define-condition shader-link-error (shader-error) ())
(define-condition shader-validate-error (shader-error) ())

(defmethod build-shader-program ((object opengl-object))
  "Compile and link a shader program, including validation."

  (with-slots (shaders program) object
    ;; Compile all shaders
    (dolist (gl-shader shaders)
      (with-slots (shader) gl-shader
        (when (and (not (zerop shader)) (not (zerop program)))
          (gl:detach-shader program shader))
        (compile-shader gl-shader)))

    ;; Create program and attach shaders
    (when (zerop program)
      (setf program (gl:create-program)))
    (dolist (shader shaders)
      (with-slots (shader) shader
        (gl:attach-shader program shader)))

    ;; Link
    (gl:link-program program)

    ;; Check for errors and validate program
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq t status))
        (error 'shader-link-error
               :status status
               :object program
               :info-log (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (restart-case
            (error 'shader-link-error
                   :status status
                   :object program
                   :info-log (gl:get-program-info-log program))
          (ignore-validation-error () t))))
    program))


(defmethod set-uniform ((obj opengl-object) name value)
  (with-slots (shaders) obj
    (dolist (shader shaders)
      (set-uniform shader name value))))

(defmethod fill-buffers ((object opengl-object))
  (with-slots (vao textures) object
    (when (/= 0 vao)
      (error "fill-buffers called on object where vao != 0"))
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)
    (dolist (texture textures)
      (fill-buffers texture))))

(defmethod update-buffers ((object opengl-object))
  (with-slots (vao textures) object
    (gl:bind-vertex-array vao)
    (dolist (texture textures)
      (update-buffers texture))))

(defmethod initialize :before ((object opengl-object))
  (cleanup object))

(defmethod initialize ((object opengl-object))
  (build-shader-program object)
  (with-slots (xform) object
    (set-uniform object "obj_transform" xform)))

(defmethod initialize :after ((object opengl-object))
  (fill-buffers object))


(defmethod cleanup ((object opengl-object))
  (with-slots (vao vbos ebos shaders program textures) object
    (when (/= 0 vao)
      (when shaders
        (dolist (shader shaders)
          (with-slots (shader) shader
            (when (and (not (zerop shader)) (not (zerop program)))
              (gl:detach-shader program shader)))
          (cleanup shader)))
      (when (> 0 program)
        (gl:delete-program program)
        (setf program 0))
      (when textures
        (dolist (texture textures)
          (cleanup texture)))
      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind-buffers ((object opengl-object))
  (with-slots (vao textures) object
    (gl:bind-vertex-array vao)
    (dolist (texture textures)
      (bind-buffers texture))))

(defmethod render ((object opengl-object))
  (with-slots (vao shaders program xform) object
    (if (zerop vao)
        (fill-buffers object)
        (bind-buffers object))
    (dolist (shader shaders)
      (use-shader-layout shader program))

    (gl:use-program program)

    (dolist (shader shaders)
      (use-shader-uniforms shader program))))

(defgeneric get-layout-descriptor (object))

(defmethod get-layout-descriptor ((object opengl-object))
  (with-slots (shaders) object

    (let ((descriptors (mapcar #'layout (remove-if-not #'identity shaders :key #'layout))))
      (if descriptors
          (progn
            (get-layout-descriptor (cadr descriptors)))
          nil))))

(defun plastic ()
  (list
   (shader-from-file (newgl-shader "color-normal-position-vertex.glsl"))
   (shader-from-file (newgl-shader "plastic-fragment.glsl"))))

(defun circled-plastic ()
  (list
   (shader-from-file (newgl-shader "uv-normal-position-vertex.glsl"))
   (shader-from-file (newgl-shader "circle-fragment.glsl"))))
