;; keyframe-axis-rotation.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass keyframe-rotation-axis-scene (scene)
  ((eye-pos :initform
            (create-keyframe-sequence (list
                                             (create-keyframe (vec3  1.0  1.0 1.0) 0.0)
                                             (create-keyframe (vec3  1.0  1.0 -1.0) 1.0)
                                             (create-keyframe (vec3 -1.0  1.0 -1.0) 2.0)
                                             (create-keyframe (vec3 -1.0  1.0 1.0) 3.0)
                                             (create-keyframe (vec3  1.0  1.0 1.0) 4.0))))))


(defmethod update :before ((scene keyframe-rotation-axis-scene) elapsed-seconds)
  (with-slots (view-xform eye-pos) scene
    (setf view-xform
          (m* (mperspective 30.0 1.0 1.0 10000.0)
              (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+)))))

(defun show-keyframe-axis-rotation ( &optional debug)
  (let ((tm (make-line-segments)))
    (add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 1 0 1)
                      :p2 (vec3 0 2 0) :c2 (vec4 0 1 0 1))
    (add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 1 0 0 1)
                      :p2 (vec3 2 0 0) :c2 (vec4 1 0 0 1))
    (add-line-2 tm
                      :p1 (vec3 0 0 0) :c1 (vec4 0 0 1 1)
                      :p2 (vec3 0 0 2) :c2 (vec4 0 0 1 1))
    (display (make-instance 'keyframe-rotation-axis-scene
                            :objects (list tm)
                            :xform (meye 4)) :debug debug)))