;; keyframe-axis-rotation.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass polygon-scene (scene)
  ((eye-pos :initform
            (create-keyframe-sequence (list
                                             (create-keyframe (vec3  1.0  1.0 1.0) 0.0)
                                             (create-keyframe (vec3  1.0  1.0 -1.0) 1.0)
                                             (create-keyframe (vec3 -1.0  1.0 -1.0) 2.0)
                                             (create-keyframe (vec3 -1.0  1.0 1.0) 3.0)
                                             (create-keyframe (vec3  1.0  1.0 1.0) 4.0))
                                      :before :repeat
                                      :after :repeat
                                      ))))


(defmethod update :before ((scene polygon-scene) elapsed-seconds)
  (with-slots (view-xform eye-pos) scene
    (setf view-xform
          (m* (mperspective 30.0 1.0 1.0 10000.0)
              (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+)))))

(defun make-polygon (sides radius color)
  (loop with ls = (make-line-segments)
        with theta-diff = (/ (* pi 2) sides)
        for i below sides
        for theta0 = (* theta-diff i)
        for theta1 = (* theta-diff (1+ i))
        do
        (add-line-2 ls
                    :p1 (vec3 (* radius (cos theta0)) (* radius (sin theta0)) 0) :c1 color
                    :p2 (vec3 (* radius (cos theta1)) (* radius (sin theta1)) 0) :c2 color)
        finally (return ls)))

(defun show-polygons ( &optional debug)
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
    (display (make-instance 'polygon-scene
                            :objects (list tm
                                           (make-polygon 7 1.0 (vec4 1 0 1 1))
                                           (make-polygon 5 0.5 (vec4 1 0 0 1))
                                           (make-polygon 3 0.25 (vec4 0 0 1 1)))
                            :xform (meye 4)) :debug debug)))

