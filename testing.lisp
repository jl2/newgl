;; scene.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass test-scene (scene)
  ((eye-pos :initform (vec3 1 1 1))
   (dir :initform (vec3 -0.1 0 0))))

(defmethod update :before ((scene test-scene))
  (with-slots (view-xform eye-pos) scene
    (setf view-xform (mlookat eye-pos (vec3 0 0 0) +vy+))))

(defun show-test-scene ()
  (let ((tm (newgl:make-line-segments)))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 1 0 1)
                      :p2 (vec3 0 1 0) :c2 (vec4 0 1 0 1))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 1 0 0 1)
                      :p2 (vec3 1 0 0) :c2 (vec4 1 0 0 1))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 0 1 1)
                      :p2 (vec3 0 0 1) :c2 (vec4 0 0 1 1))
    (display (make-instance 'test-scene
                            :objects (list tm)
                            :xform (meye 4)))))
