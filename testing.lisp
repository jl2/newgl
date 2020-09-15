;; scene.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass test-scene (scene)
  ((eye-pos :initform
            (newgl:create-keyframe-sequence (list
                                             (newgl:create-keyframe (vec3  1.0  1.0 1.0) 0.0)
                                             (newgl:create-keyframe (vec3  1.0  1.0 -10.0) 1.0)
                                             (newgl:create-keyframe (vec3 -10.0  1.0 -10.0) 2.0)
                                             (newgl:create-keyframe (vec3 -10.0  1.0 1.0) 4.0)
                                             (newgl:create-keyframe (vec3  1.0  1.0 1.0) 8.0))))
   ))

(defmethod update :before ((scene test-scene) elapsed-seconds)
  (with-slots (view-xform eye-pos) scene
    (setf view-xform
          (m*
           (mperspective 30.0 1.0 1.0 10000.0)
           (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+)
           ))))

(defun show-test-scene ()
  (let ((tm (newgl:make-line-segments)))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 1 0 1)
                      :p2 (vec3 0 2 0) :c2 (vec4 0 1 0 1))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 1 0 0 1)
                      :p2 (vec3 2 0 0) :c2 (vec4 1 0 0 1))
    (newgl:add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 0 1 1)
                      :p2 (vec3 0 0 2) :c2 (vec4 0 0 1 1))
    (display (make-instance 'test-scene
                            :objects (list tm)
                            :xform (meye 4)) :debug nil)))
