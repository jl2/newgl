;;;; opengl-object.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass quad (vertex-object)
  ((newgl:vertices :initform (make-array
                              20
                              :element-type 'single-float
                              :initial-contents (list
                                                 -1.0f0  1.0f0  0.0f0 0.0f0 1.0f0
                                                 -1.0f0 -1.0f0  0.0f0 0.0f0 0.0f0
                                                 1.0f0  1.0f0  0.0f0 1.0f0 1.0f0
                                                 1.0f0 -1.0f0  0.0f0 1.0f0 0.0f0))
                   :initarg :vertices)
   (newgl:indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2))))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defun make-uv-quad (shader-program u-min u-max v-min v-max)
  (let ((verts (list
                -1.0f0  1.0f0 0.0f0
                u-min v-max
                -1.0f0 -1.0f0 0.0f0
                u-min v-min
                1.0f0  1.0f0 0.0f0
                u-max v-max
                1.0f0 -1.0f0 0.0f0
                u-max v-min)))
    (make-instance
     'quad
     :vertices (make-array
                (length verts)
                :element-type 'single-float
                :initial-contents
                (mapcar (lambda (x) (coerce x 'single-float)) verts))
     :shader-program shader-program)))

;; (defun make-uv-quad (u-min u-max v-min v-max)
;;   (make-instance 'quad :vertices (make-array
;;                               20
;;                               :element-type 'single-float
;;                               :initial-contents
;;                               (list
;;                                -1.0f0  1.0f0 0.0f0
;;                                (coerce u-min 'single-float) (coerce v-max 'single-float)
;;                                -1.0f0 -1.0f0 0.0f0
;;                                (coerce u-min 'single-float) (coerce v-min 'single-float)
;;                                1.0f0  1.0f0 0.0f0
;;                                (coerce u-max 'single-float) (coerce v-max 'single-float)
;;                                1.0f0 -1.0f0 0.0f0
;;                                (coerce u-max 'single-float) (coerce v-min 'single-float)))))
