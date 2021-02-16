;; keyframe-viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl)

(defparameter *radius* 8.0)

(defclass keyframe-viewer (viewer)
  ((eye-pos :initarg :eye-pos)
   (look-at :initarg :look-at :initform (vec3 0.0 0.0 0.0))
   (up-vector :initarg :up-vector :initform +vy+)))

(defmethod update-view-xform ((viewer keyframe-viewer) elapsed-seconds)
  (call-next-method)
  (with-slots (view-xform eye-pos aspect-ratio) viewer
    (setf view-xform
          (m* (mperspective 30.0 aspect-ratio 1.0 10000.0)
              (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+))))
  t)

(defun rotating-display (objects &key
                                       (radius 4.0)
                                       (cam-height (/ radius 2))
                                       (segments 180)
                                       (dt (/ 0.125 16)))
  (display (make-instance 'keyframe-viewer
                 :objects (ensure-list objects)
                 :eye-pos (create-keyframe-sequence
                           (loop
                                 with theta-diff = (/ (* 2 pi) segments)
                                 for i below (1+ segments)
                                 for theta0 = (* theta-diff i)
                                 for pt = (vec3 (* radius (cos (- theta0)))
                                                cam-height
                                                (* radius (sin (- theta0))))
                                 collect (create-keyframe pt (* dt i)))
                           :before :repeat
                           :after :repeat)
                 :look-at (vec3 0 0 0))))
