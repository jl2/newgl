;; opengl-object.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (name :initform "GL Object" :initarg :name)
   (style :initform (point-style)
          :type style
          :accessor style
          :initarg :style)
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
   (instance-count :initform 1
                   :initarg :instance-count)
   (active-style :initform nil
                 :accessor active-style
                 :initarg :active-style))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defclass instanced-opengl-object (opengl-object)
  ((instance-count :initform 1 :initarg :instance-count)))

(defgeneric build-style (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))

(defgeneric set-uniform (object name value type &key overwrite)
  (:documentation "Set a uniform variable on object."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))

(defmethod show-info ((object opengl-object) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (declare (ignorable plus-plus-ws plus-ws))
    (format t "~aObject:~%" this-ws)
    (show-slots plus-ws object '(name vao style textures instance-count))

    (format t "~a~a~%" this-ws (name (style object)))
    (show-info (style object) :indent (1+ indent))
    (dolist (buffer (buffers object))
      (format t "~a~a~%" this-ws (car buffer))
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

(defmethod handle-3d-mouse-event ((object opengl-object) (event sn:motion-event))
  (declare (ignorable object event))
  nil)

(defmethod update ((object opengl-object) elapsed-seconds )
  (declare (ignorable object elapsed-seconds))
  nil)

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
  (with-slots (vao) object
    (when (/= 0 vao)
      (error "initialize called on object where vao != 0 ~a" object))

    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (initialize-style object)

    (initialize-buffers object)
    (initialize-uniforms object)
    (initialize-textures object)))

(defmethod initialize-style ((object opengl-object) &key)
  (with-slots (style) object
    (build-style style)))

(defmethod initialize-uniforms ((object opengl-object) &key)
  t)

(defmethod initialize-buffers ((object opengl-object) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (set-buffer object
              :vertices
              (make-instance
               'attribute-buffer
               :pointer (to-gl-array
                         :float
                         21
                         (list -0.5f0 -0.5f0 0.0f0
                               0.0f0 1.0f0 0.0f0 1.0f0

                               0.5f0 -0.5f0 0.0f0
                               0.0f0 1.0f0  0.0f0 1.0f0

                               0.0f0 (- (sqrt (- 1.0f0 (* 0.5f0 0.5f0))) 0.5f0)  0.0f0
                               0.0f0 1.0f0 0.0f0 1.0f0))
               :stride nil
               :attributes '(("in_position" . :vec3) ("in_color" . :vec4))
               :usage :static-draw
               :free nil))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 3
               :pointer (to-gl-array :unsigned-int 3 #(0 1 2))
               :stride nil
               :usage :static-draw
               :free nil))
  (set-buffer object
              :transform (make-instance
                          'instance-buffer
                          :pointer (to-gl-array :float 16 (meye 4))
                          :stride nil
                          :usage :static-draw
                          :free t))
  (set-buffer object
              :transform (make-instance
                          'instance-buffer
                          :pointer (to-gl-array :float 4 (vec4 0.0 1.0 0.0 1.0))
                          :stride nil
                          :attributes '(("in_color" . :vec4))
                          :usage :static-draw
                          :free t)))


(defmethod initialize-textures ((object opengl-object) &key)
  (declare (ignorable object))
  nil)

(defmethod cleanup ((object opengl-object))
  (with-slots (vao buffers textures uniforms style) object

    (when (/= 0 vao)
      (gl:bind-vertex-array vao)

      (when textures
        (dolist (texture textures)
          (cleanup texture)))

      (when uniforms
        (dolist (uniform uniforms)
          (cleanup (cdr uniform))))

      (when buffers
        (dolist (buffer buffers)
          (cleanup (cdr buffer))))

      (when style
        (cleanup style))

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
  (with-slots (buffers uniforms primitive-type instance-count style) object
    (bind object)
    (use-style style)

    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))

    (when (> instance-count 0)
      (gl:draw-elements primitive-type
                        (gl:make-null-gl-array :unsigned-int)
                        :count (idx-count (assoc-value buffers :indices))))
    (unuse-style style)))

(defmethod render ((object instanced-opengl-object))
  (with-slots (buffers uniforms primitive-type instance-count style) object
    (bind object)
    (use-style style)
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))


    (when (> instance-count 0)
      (gl:draw-elements-instanced  primitive-type
                                   (gl:make-null-gl-array :unsigned-int)
                                   instance-count
                                   :count (idx-count (assoc-value buffers :indices))))
    (unuse-style style)))

(defun set-buffer (object buffer-name buffer)
  (declare (type opengl-object object)
           (type buffer buffer))
  (with-slots (buffers style idx-count) object
    (if-let  ((location (assoc buffer-name buffers)))
      (progn
        (cleanup (cdr location))
        (rplacd location buffer))

      (push (cons buffer-name buffer) buffers))

    (bind buffer)
    (associate-attributes buffer (program style))))

(defun set-style (object new-style)
  (declare (type opengl-object object)
           (type style new-style))
  (with-slots (style) object
    (when style
      (cleanup style))
    (setf style new-style)))

(defun get-buffer (object buffer-name)
  (assoc (buffers object) buffer-name))

(defun use-texture (object texture)
  (declare (type opengl-object object)
           (type texture texture))
  (push texture (textures object)))

