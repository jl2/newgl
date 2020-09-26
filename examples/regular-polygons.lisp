;; keyframe-axis-rotation.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defparameter *radius* 8.0)


(defun make-polygon (sides radius color
                     &key
                       (position (vec3 0 0 0))
                       (x-rotate 0)
                       (y-rotate 0)
                       (z-rotate 0))
  (loop with ls = (make-line-segments)
        with theta-diff = (/ (* pi 2) sides)
        for i below sides
        for theta0 = (* theta-diff i)
        for theta1 = (* theta-diff (1+ i))
        do
        (add-line-2 ls
                    :p1 (vec3 (* radius (cos theta0)) (* radius (sin theta0)) 0) :c1 color
                    :p2 (vec3 (* radius (cos theta1)) (* radius (sin theta1)) 0) :c2 color)
        finally (progn
                  ;; (with-slots (xform) ls
                  ;;   (nm* xform
                  ;;        (mrotation +vx+ x-rotate)
                  ;;        (mrotation +vy+ y-rotate)
                  ;;        (mrotation +vz+ z-rotate)
                  ;;        (mtranslation position)
                  ;;        ))
                   (return ls))))

(defun show-polygons ( &optional debug)
  (display-in-rotating-scene (list (create-axis 5 :half nil)
                                   (make-polygon 8 4.0 (vec4 1 0 1 1)
                                                 :position (3d-vectors:vec3-random -5 5))
                                   (make-polygon 7 2.0 (vec4 1 0 1 1)
                                                 :position (3d-vectors:vec3-random -5 5))
                                   (make-polygon 6 1.0 (vec4 1 0 1 1)
                                                 :position (3d-vectors:vec3-random -5 5)
                                                 :y-rotate (/ pi 3))
                                   (make-polygon 5 0.5 (vec4 1 0 0 1)
                                                 :position (3d-vectors:vec3-random -5 5))
                                   (make-polygon 4 0.25 (vec4 1 0 0 1)
                                                 :position (3d-vectors:vec3-random -5 5))
                                   (make-polygon 3 0.125 (vec4 0 0 1 1)
                                                 :position (3d-vectors:vec3-random -5 5)))
                             :radius 12 :cam-height 6
                             :debug debug))
