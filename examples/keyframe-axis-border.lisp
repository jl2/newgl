;; keyframe-axis-rotation.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass keyframe-axis-border-viewer (viewer)
  ((target-pos :initform
               (create-keyframe-sequence (list
                                          (create-keyframe (vec3  1.0  0.0 1.0) 0.0)
                                          (create-keyframe (vec3  1.0  0.0 -1.0) 1.0)
                                          (create-keyframe (vec3 -1.0  0.0 -1.0) 2.0)
                                          (create-keyframe (vec3 -1.0  0.0 1.0) 3.0)
                                          (create-keyframe (vec3  1.0  0.0 1.0) 4.0))
                                         :before :repeat
                                         :after :repeat))))


(defmethod update :before ((viewer keyframe-axis-border-viewer) elapsed-seconds)
  (with-slots (view-xform target-pos) viewer
    (setf view-xform
          (m* (mperspective 30.0 1.0 1.0 10000.0)
              (mlookat (vec3 10 10 10) (value-at target-pos elapsed-seconds) +vz+)))))

(defun show-keyframe-axis-border ( &optional debug)
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
    (display (make-instance 'keyframe-axis-border-viewer
                            :objects (list tm)
                            :xform (meye 4)) :debug debug)))
