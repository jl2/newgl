;; opengl-object.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass st-quad (opengl-object)
  ((s-min :initarg :s-min :initform -1.0f0)
   (t-min :initarg :t-min :initform -1.0f0)
   (s-max :initarg :s-max :initform 1.0f0)
   (t-max :initarg :t-max :initform 1.0f0)
   (primitive-type :initform :triangles)
   )
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod initialize-buffers ((object st-quad) &key)
  (when (buffers object)
    (error "Initializing an object that's already initialized! Cleanup first! ~a" object))
  (with-slots (s-min s-max t-min t-max) object
    (use-buffer object
              (make-instance
               'attribute-buffer
               :pointer (to-gl-array
                         :float
                         20
                         `#(-1.0f0  1.0f0 0.0f0
                            ,s-min ,t-max

                            -1.0f0 -1.0f0 0.0f0
                            ,s-min ,t-min

                            1.0f0  1.0f0 0.0f0
                            ,s-max ,t-max

                            1.0f0 -1.0f0 0.0f0
                            ,s-max ,t-min))
               :stride nil
               :attributes '(("in_position" . :vec3) ("in_uv" . :vec2))
               :usage :static-draw
               :free nil)))
  (use-buffer object
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2 1 3 2))
               :stride nil
               :usage :static-draw
               :free nil)))

(defun make-st-quad (&key
                       (s-min -1.0)
                       (s-max 1.0)
                       (t-min -1.0)
                       (t-max 1.0)
                       shaders
                       textures)
  (when (every (compose #'not #'null) (list s-min s-max t-min t-max))
    (make-instance 'st-quad
                   :s-min (coerce s-min 'single-float)
                   :s-max (coerce s-max 'single-float)
                   :t-min (coerce t-min 'single-float)
                   :t-max (coerce t-max 'single-float)
                   :shaders shaders
                   :textures textures)))
