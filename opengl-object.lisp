;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defgeneric bind (object))

(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value type)
  (:documentation "Set a uniform variable on object."))


(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (name :initform "GL Object" :initarg :name)
   (xform :initform (meye 4) :initarg :xform :type mat4)
   ;; (shaders :initarg :shaders :initform nil :type (or null list))
   ;; (textures :initarg :textures :initform nil :type (or null list))
   ;; (buffers :initarg :buffers :initform nil :type (or null list))
   ;; (uniforms :initarg :uniforms :initform nil :type (or null list))
   ;; (primitive-type :initarg :primitive-type :initform :triangles)

   (shaders :initform (newgl:point-shader) :type (or null list))
   (textures :initform nil :type (or null list))
   (buffers :initform nil :type (or null list))
   (uniforms :initform nil :type (or null list))
   (primitive-type :initform :points)
   (program :initform 0)
   (idx-count :initform 2))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))

(defmethod show-info ((object opengl-object) &key (indent ""))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (format t "~aObject:~%" this-ws)
    (dolist (slot '(name vao xform shaders textures program))
      (format t "~a~a: ~a~%" plus-ws slot (slot-value object slot)))

    (with-slots (program) object
      (let ((gl-shaders (gl:get-attached-shaders program)))
        (format t "~aOpenGL Shaders: ~a~%" plus-ws gl-shaders)
        (dolist (shader gl-shaders)
        (format t "~aShader ~a~%" plus-ws shader)
        (dolist (attrib '(:shader-type
                          :delete-status :compile-status :info-log-length :shader-source-length))
          (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-shader shader attrib))))))
  (with-slots (buffers) object
    (dolist (buffer buffers)
      (show-info buffer :indent (1+ indent))))))
  
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
  (declare (ignorable object elapsed-seconds))
  nil)

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
        (initialize gl-shader)))

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


(defmethod set-uniform ((obj opengl-object) name value type)
  (with-slots (uniforms) obj
    (if-let (previous (assoc name uniforms :test #'string=))
      (set-value (cdr previous) value)
          (push (cons name (make-instance 'uniform :name name
                                                   :type type
                                                   :value value))
                uniforms))))

(defmethod initialize :before ((object opengl-object) &key)
  (cleanup object))

(defmethod initialize ((object opengl-object) &key)

  (build-shader-program object)

  (with-slots (vao textures buffers) object
    (when (/= 0 vao)
      (error "fill-buffers called on object where vao != 0"))

    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)
    (initialize-buffers object)
    (initialize-uniforms object)
    (initialize-textures object)))


(defmethod cleanup ((object opengl-object))
  (with-slots (vao program shaders buffers textures) object

    (when (/= 0 vao)

      (gl:bind-vertex-array vao)

      (when shaders

        (dolist (shade shaders)
          (with-slots (shader) shade
            (when (and (not (zerop shader))
                       (not (zerop program)))
              (gl:detach-shader program shader)))
          (cleanup shade)))

      (when (> 0 program)
        (gl:delete-program program)
        (setf program 0))

      (when textures
        (dolist (texture textures)
          (cleanup texture)))
      (setf textures nil)

      (when buffers
        (dolist (buffer buffers)
          (cleanup buffer)))
      (setf buffers nil)


      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind ((object opengl-object))
  (with-slots (vao buffers textures) object
    (if (= vao 0)
        (error "Trying to bind an uninitialized opengl-object!")
        (gl:bind-vertex-array vao))
    (dolist (texture textures)
      (bind texture))))

(defmethod render ((object opengl-object))
  (with-slots (vao shaders program uniforms primitive-type idx-count) object
    (gl:use-program program)
    (bind object)
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) program))

    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count idx-count)))

(defmethod initialize-buffers ((object opengl-object))
  (with-slots (buffers idx-count program) object
    (when buffers
      (error "Initializing an object that's already initialized! Cleanup first! ~a" object))
    (let ((vertices
            (make-instance
             'attribute-buffer
             :count 2
             :pointer (to-gl-array :float #(0.0 0.0 0.0 0.0 1.0 0.0 1.0
                                            0.5 0.0 0.0 0.0 0.0 1.0 1.0))
             :stride 7
             :attributes '(("in_position" . :vec3) ("in_color" . :vec4))
             :usage :static-draw
             :free nil))
          (indices
            (make-instance
             'index-buffer
             :count 2
             :pointer (to-gl-array :unsigned-int #(0 1))
             :stride 1
             :target :element-array-buffer
             :usage :static-draw
             :free nil)))
      (push vertices buffers)
      (push indices buffers))
    (dolist (buffer buffers)
      (initialize buffer)
      (associate-attributes buffer program))
    (setf idx-count 2)))

(defmethod initialize-textures ((object opengl-object))
  nil)

(defmethod initialize-uniforms ((object opengl-object))
  (with-slots (xform) object
    (set-uniform object "obj_transform" xform :mat4)))
