;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass st-quad (geometry)
  ((s-min :initarg :s-min)
   (t-min :initarg :t-min)
   (s-max :initarg :s-max)
   (t-max :initarg :t-max))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod allocate-and-fill-buffers ((object st-quad))
  (with-slots (s-min s-max t-min t-max) object
    (values (to-gl-array :float
                         (list
                          -1.0f0  1.0f0 0.0f0
                                0.0f0 0.0f0 1.0f0
                                s-min t-max
                                -1.0f0 -1.0f0 0.0f0
                                0.0f0 0.0f0 1.0f0
                                s-min t-min
                                1.0f0  1.0f0 0.0f0
                                0.0f0 0.0f0 1.0f0
                                s-max t-max
                                1.0f0 -1.0f0 0.0f0
                                0.0f0 0.0f0 1.0f0
                                s-max t-min))
            (to-gl-array :unsigned-int #(0 1 2 1 3 2)))))

(defun make-st-quad (&key s-min s-max t-min t-max shaders)
  (when (every (compose #'not #'null) (list s-min s-max t-min t-max))
    (make-instance 'st-quad
                   :s-min (coerce s-min 'single-float)
                   :s-max (coerce s-max 'single-float)
                   :t-min (coerce t-min 'single-float)
                   :t-max (coerce t-max 'single-float)
                   :shaders shaders)))
