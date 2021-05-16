;; opengl-object.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass st-quad (opengl-object)
  ((s-min :initarg :s-min :initform 0.0f0)
   (t-min :initarg :t-min :initform 0.0f0)
   (s-max :initarg :s-max :initform 1.0f0)
   (t-max :initarg :t-max :initform 1.0f0)
   (primitive-type :initform :triangles)
   )
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod initialize-buffers ((object st-quad) &key)
  (with-slots (s-min s-max t-min t-max) object
    (set-buffer object
                :vertices
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
               :attributes '(("in_position" . :vec3) ("in_uv" . :vec2))
               :free t)))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2 1 3 2))
               :free t))
  (set-buffer object :transforms (make-instance 'instance-buffer
                                                   :pointer (to-gl-array :float 16 (list (meye 4)))
                                                   :free t))
  (set-buffer object :color (make-instance 'instance-buffer
                                                   :pointer (to-gl-array :float 4 (vec4 0.1 1.0 0.1 1.0))
                                                   :free t
                                                   :attributes '(("in_color" . :vec4)))))

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
