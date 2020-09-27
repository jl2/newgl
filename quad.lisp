;; opengl-object.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass quad (geometry)
  ((u-min :initarg :u-min)
   (v-min :initarg :v-min)
   (u-max :initarg :u-max)
   (v-max :initarg :v-max))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod vertex-buffers ((object quad))
  (with-slots (u-min v-min u-max v-max) object
    (let ((verts (list
                  -1.0f0  1.0f0 0.0f0
                  u-min v-max
                  -1.0f0 -1.0f0 0.0f0
                  u-min v-min
                  1.0f0  1.0f0 0.0f0
                  u-max v-max
                  1.0f0 -1.0f0 0.0f0
                  u-max v-min)))
      (values (make-array
               (length verts)
               :element-type 'single-float
               :initial-contents
               (mapcar (lambda (x) (coerce x 'single-float)) verts))
              (make-array
               6
               :element-type 'fixnum
               :initial-contents '(0 1 2 1 3 2))))))

(defun make-uv-quad (shader-program u-min u-max v-min v-max)
  (make-instance
   'quad
     :u-min u-min
     :u-max u-max
     :v-min v-min
     :v-max v-max
     :shader-program shader-program))
