;; keyframe-viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl)

(defparameter *radius* 8.0)

(defun polygon-list (sides radius)
  (loop
        with theta-diff = (/ (* pi 2) sides)
        for i below (1+ sides)
        for theta0 = (* theta-diff i)
        collect (vec3 (* radius (cos theta0))
                      0
                      (* radius (sin theta0)))))

(defclass keyframe-viewer (viewer)
  ((eye-pos :initarg :eye-pos)
   (look-at :initarg :look-at :initform (vec3 0.0 0.0 0.0))
   (up-vector :initarg :up-vector :initform +vy+)))

(defun create-rotating-keyframe-viewer (radius objects &key
                                                    (cam-height 8.0)
                                                    (segments 180)
                                                    (dt (/ 0.125 16)))
  (make-instance 'keyframe-viewer
                 :objects objects
                 :eye-pos (create-keyframe-sequence
                           (loop for i from 0
                                 for j = -1 then (- j)
                                 for pt in (polygon-list segments radius)
                                 collect (create-keyframe (v+ (vec3 0.0 cam-height 0.0) pt)
                                                          (* dt i)))
                           :before :repeat
                           :after :repeat)))

(defmethod update :before ((viewer keyframe-viewer) elapsed-seconds)
  (with-slots (view-xform eye-pos aspect-ratio) viewer
    (setf view-xform
          (m* (mperspective 30.0 aspect-ratio 1.0 10000.0)
              (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+)))))

(defun display-in-rotating-viewer (object &key
                                           (radius 4.0)
                                           (cam-height (/ radius 2))
                                           (segments 180)
                                           (dt (/ 0.125 16))
                                           (debug nil))
    (display (create-rotating-keyframe-viewer radius
                                             (cons (create-axis 2 :half nil)
                                                   (ensure-list object))
                                             :cam-height cam-height
                                             :segments segments
                                             :dt dt)
             :debug debug))



