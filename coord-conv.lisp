;; coord-conv.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)


(defun cartesian-2-polar (vec-2)
  (let ((radius (vlength vec-2)))
    (vec2 radius
          (atan (vy vec-2) (vx vec-2)))))

(defun polar-2-cartesian (vec-2)
  (let ((radius (vx vec-2))
        (theta (vy vec-2)))
    (vec2 (* radius (cos theta))
          (* radius (sin theta)))))


(defun cartesian-2-spherical (vec-3)
  (let ((radius (vlength vec-3)))
    (vec3 radius
          (if (zerop radius)
              0.0
              (acos (/ (vz vec-3)
                       radius)))
          (atan (vy vec-3)
                (vx vec-3))
          )))

(defun spherical-2-cartesian (vec-3)
  (let ((r (vx vec-3))
        (theta (vy vec-3))
        (phi (vz vec-3)))
    (vec3 (* r (cos phi) (sin theta))
          (* r (sin phi) (sin theta))
          (* r (cos theta)))))


(defun cartesian-2-cylindrical (vec-3)
  (let ((radius (vlength (vxy vec-3))))
    (vec3 radius
          (atan (vy vec-3) (vx vec-3))
          (vz vec-3))))

(defun cylindrical-2-cartesian (vec-3)
  (let ((radius (vx vec-3))
        (phi (vy vec-3)))
    (vec3 (* radius (cos phi))
          (* radius (sin phi))
          (vz vec-3))))

(defun spherical-2-cylindrical (vec-3)
  (let ((radius (vx vec-3))
        (theta (vy vec-3))
        (phi (vz vec-3)))
    (vec3 (* radius (sin theta))
          phi
          (* radius (cos theta)))))

(defun cylindrical-2-spherical (vec-3)
  (let ((radius (vlength (vxy vec-3))))
    (vec3 radius
          (if (zerop radius)
              0.0
              (acos (/ (vz vec-3) radius)))
          (vy vec-3))))
