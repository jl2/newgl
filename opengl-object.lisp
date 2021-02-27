;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (name :initform "GL Object" :initarg :name)
   (program :initform 0
            :accessor program)

   (shaders :initform nil
            :type (or null list)
            :accessor shaders
            :initarg :shaders)
   (textures :initform nil
             :type (or null list)
             :accessor textures
             :initarg :textures)
   (buffers :initform nil
            :type (or null list)
            :accessor buffers
            :initarg :buffers)
   (uniforms :initform nil
             :type (or null list)
             :accessor uniforms
             :initarg :uniforms)

   (primitive-type :initform :triangles)

   (idx-count :initform 4))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric build-shader-program (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value type &key overwrite)
  (:documentation "Set a uniform variable on object."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))

(defmethod show-info ((object opengl-object) &key (indent ""))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (format t "~aObject:~%" this-ws)
    (dolist (slot '(name vao shaders textures program))
      (format t "~a~a: ~a~%" plus-ws slot (slot-value object slot)))

    (with-slots (program) object
      (dolist (attrib '(:delete-status
                        :link-status
                        :validate-status
                        :info-log-length
                        :attached-shaders
                        :active-atomic-counter-buffers
                        :active-attributes
                        :active-attribute-max-length
                        :active-uniforms
                        :active-uniform-blocks
                        :active-uniform-block-max-name-length
                        :active-uniform-max-length
                        ;; :compute-work-group-size
                        :program-binary-length
                        :transform-feedback-buffer-mode
                        :transform-feedback-varyings
                        :transform-feedback-varying-max-length
                        ;; :geometry-vertices-out
                        ;; :geometry-input-type
                        ;; :geometry-output-type
                        ))
        (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-program program attrib)))
      (dotimes (idx (gl:get-program program :active-attributes))
        (multiple-value-bind (size type name) (gl:get-active-attrib program idx)
          (format t "~aAttrib ~a ~a size ~a~%" plus-plus-ws name type size)))


      (let ((gl-shaders (gl:get-attached-shaders program)))
        (format t "~aOpenGL Shaders: ~a~%" plus-ws gl-shaders)
        (dolist (shader gl-shaders)
        (format t "~aShader ~a~%" plus-ws shader)
        (dolist (attrib '(:shader-type
                          :delete-status :compile-status :info-log-length :shader-source-length))
          (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-shader shader attrib))))))
    (dolist (buffer (buffers object))
      (show-info (cdr buffer) :indent (1+ indent)))
    (dolist (uniform (uniforms object))
      (show-info (cdr uniform) :indent (1+ indent)))))

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

  (with-slots (program) object
    ;; Compile all shaders
    (when (zerop program)
      (setf program (gl:create-program)))

    (dolist (gl-shader (shaders object))
      (when (and (not (zerop (shader gl-shader))) (not (zerop program)))
        (gl:detach-shader program (shader gl-shader)))
      (initialize gl-shader)
      (gl:attach-shader program (shader gl-shader)))

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


(defmethod set-uniform ((obj opengl-object) name value type &key (overwrite t))
  (with-slots (uniforms) obj
    (if-let (previous (assoc name uniforms :test #'string=))
      (when overwrite
        (set-value (cdr previous) value))
      (push (cons name (make-instance 'uniform :name name
                                               :type type
                                               :value value))
            uniforms))))

(defmethod initialize :before ((object opengl-object) &key)
  (cleanup object))

(defmethod initialize ((object opengl-object) &key)
  (with-slots (vao buffers) object
    (when (/= 0 vao)
      (error "initialize called on object where vao != 0 ~a" object))

    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (initialize-shaders object)

    (initialize-buffers object)
    (initialize-uniforms object)
    (initialize-textures object)))

(defmethod initialize-shaders ((object opengl-object) &key)
  (when (null (shaders object))
    (setf (shaders object) (newgl:point-shader)))
  (build-shader-program object))

(defmethod initialize-uniforms ((object opengl-object) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4 :overwrite nil))

(defmethod initialize-buffers ((object opengl-object) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (add-buffer object
              (make-instance
               'attribute-buffer
               :count (* 3 7)
               :pointer (to-gl-array
                         :float
                         `#(-0.5f0 -0.5f0 0.0f0
                            0.0f0 1.0f0 0.0f0 1.0f0

                            0.5f0 -0.5f0 0.0f0
                            0.0f0 1.0f0  0.0f0 1.0f0

                            0.0f0 ,(- (sqrt (- 1 (* 0.5 0.5))) 0.5)  0.0f0
                            0.0f0 1.0f0 0.0f0 1.0f0))
               :stride nil
               :attributes '(("in_position" . :vec3) ("in_color" . :vec4))
               :usage :static-draw
               :free nil))
  (add-buffer object
              (make-instance
               'index-buffer
               :count 3
               :pointer (to-gl-array :unsigned-int #(0 1 2))
               :stride nil
               :usage :static-draw
               :free nil)))

(defmethod initialize-textures ((object opengl-object) &key)
  (declare (ignorable object))
  nil)

(defmethod cleanup ((object opengl-object))
  (with-slots (vao program shaders buffers textures uniforms) object

    (when (/= 0 vao)
      (gl:bind-vertex-array vao)

      (when (> 0 program)
        (gl:delete-program program)
        (setf program 0))

      (when textures
        (dolist (texture textures)
          (cleanup texture)))
      (setf textures nil)

      (when uniforms
        (dolist (uniform uniforms)
          (cleanup (cdr uniform))))

      (when buffers
        (dolist (buffer buffers)
          (cleanup (cdr buffer))))
      (setf buffers nil)

      (when shaders
        (dolist (shade shaders)
          (with-slots (shader) shade
            (when (and (not (zerop shader))
                       (not (zerop program)))
              (gl:detach-shader program shader)))
          (cleanup shade)))

      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind ((object opengl-object))
  (with-slots (vao buffers textures) object
    (if (= vao 0)
        (error "Trying to bind an uninitialized opengl-object!")
        (gl:bind-vertex-array vao))
    (dolist (buffer buffers)
      (bind (cdr buffer)))
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

(defun add-buffer (object buffer)
  (declare (type opengl-object object)
           (type buffer buffer))
  (with-slots (target count) buffer
    (with-slots (buffers idx-count program) object
      (push (cons target buffer) buffers)
      (when (eq target :element-array-buffer)
        (setf idx-count (slot-value buffer 'count)))
      (bind buffer)
      (associate-attributes buffer program))))

(defun add-texture (object texture)
  (declare (type opengl-object object)
           (type texture texture))
  (push texture (textures object)))

(defun add-shader (object shader)
  (declare (type opengl-object object)
           (type gl-shader shader))
  (push shader (shaders object)))

