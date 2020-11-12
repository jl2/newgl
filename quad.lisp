;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass uv-quad (geometry)
  ((u-min :initarg :u-min)
   (v-min :initarg :v-min)
   (u-max :initarg :u-max)
   (v-max :initarg :v-max))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod allocate-and-fill-buffers ((object uv-quad))
  (with-slots (u-min u-max v-min v-max) object
    (values (to-gl-float-array (list
                                -1.0f0  1.0f0 0.0f0
                                u-min v-max
                                -1.0f0 -1.0f0 0.0f0
                                u-min v-min
                                1.0f0  1.0f0 0.0f0
                                u-max v-max
                                1.0f0 -1.0f0 0.0f0
                                u-max v-min))
            (to-gl-array #(0 1 2 1 3 2) :unsigned-int))))

(defun make-uv-quad (&key u-min u-max v-min v-max shaders)
  (when (every (compose #'not #'null) (list u-min u-max v-min v-max))
    (make-instance 'uv-quad
                   :u-min u-min
                   :u-max u-max
                   :v-min v-min
                   :v-max v-max
                   :shaders shaders)))
