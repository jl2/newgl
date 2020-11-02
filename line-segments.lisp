;; point-cloud.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass line-segments (geometry)
  ((vertices :initform (make-array 0
                                   :element-type 'single-float
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (indices :initform (make-array 0
                                  :element-type 'fixnum
                                  :initial-contents '()
                                  :adjustable t
                                  :fill-pointer 0))
   (primitive-type :initform :lines)
   (shaders :initform (list 
                       (shader-from-file (newgl-shader "point-vertex.glsl"))
                       (shader-from-file (newgl-shader "point-fragment.glsl")))))
  (:documentation "Point cloud."))

(defmethod allocate-and-fill-buffers ((object line-segments))
  (with-slots (vertices indices) object
    (values (to-gl-float-array vertices)
            (to-gl-array indices :unsigned-int))))

(defun add-line-2 (lines
                   &key
                 p1
                 c1
                 p2
                 c2)
  (with-slots (vertices indices) lines

    (let ((index  (length indices)))
      (vector-push-extend (vx p1) vertices)
      (vector-push-extend (vy p1) vertices)
      (vector-push-extend (vz p1) vertices)
      (vector-push-extend (vx c1) vertices)
      (vector-push-extend (vy c1) vertices)
      (vector-push-extend (vz c1) vertices)
      (vector-push-extend (vw c1) vertices)

      (vector-push-extend (vx p2) vertices)
      (vector-push-extend (vy p2) vertices)
      (vector-push-extend (vz p2) vertices)
      (vector-push-extend (vx c2) vertices)
      (vector-push-extend (vy c2) vertices)
      (vector-push-extend (vz c2) vertices)
      (vector-push-extend (vw c2) vertices)
      (vector-push-extend index indices)
      (vector-push-extend (1+ index) indices)
      (values index (1+ index)))))

(defun add-line (lines &key
                         x1 y1 z1
                         x2 y2 z2
                         (r1 1.0f0) (g1 1.0f0) (b1 1.0f0) (a1 1.0f0)
                         (r2 r1) (g2 g1) (b2 b1) (a2 a1))
  (with-slots (vertices indices) lines
    (let ((index  (length indices)))
      (vector-push-extend (coerce x1 'single-float) vertices)
      (vector-push-extend (coerce y1 'single-float) vertices)
      (vector-push-extend (coerce z1 'single-float) vertices)
      (vector-push-extend (coerce r1 'single-float) vertices)
      (vector-push-extend (coerce g1 'single-float) vertices)
      (vector-push-extend (coerce b1 'single-float) vertices)
      (vector-push-extend (coerce a1 'single-float) vertices)
      (vector-push-extend (coerce x2 'single-float) vertices)
      (vector-push-extend (coerce y2 'single-float) vertices)
      (vector-push-extend (coerce z2 'single-float) vertices)
      (vector-push-extend (coerce r2 'single-float) vertices)
      (vector-push-extend (coerce g2 'single-float) vertices)
      (vector-push-extend (coerce b2 'single-float) vertices)
      (vector-push-extend (coerce a2 'single-float) vertices)

      (vector-push-extend index indices)
      (vector-push-extend (1+ index) indices)
      (values index (1+ index)))))

(defun add-line-by-index (lines idx0 idx1)
  (with-slots (indices) lines
    (vector-push-extend idx0 indices)
    (vector-push-extend idx1 indices)
    (values idx0 idx1)))

(defun add-line-by-pt-index-2 (lines idx0 p1 c1)
  (with-slots (vertices indices) lines
    (let ((index  (length indices)))
      (vector-push-extend idx0 indices)

      (vector-push-extend (vx p1) vertices)
      (vector-push-extend (vy p1) vertices)
      (vector-push-extend (vz p1) vertices)
      (vector-push-extend (vx c1) vertices)
      (vector-push-extend (vy c1) vertices)
      (vector-push-extend (vz c1) vertices)
      (vector-push-extend (vw c1) vertices)
      (vector-push-extend index indices)
      (values idx0 index))))

(defun add-line-by-pt-index (lines idx0 x y z r g b a)
  (with-slots (vertices indices) lines
    (let ((index  (length indices)))
      (vector-push-extend idx0 indices)
      (vector-push-extend (coerce x 'single-float) vertices)
      (vector-push-extend (coerce y 'single-float) vertices)
      (vector-push-extend (coerce z 'single-float) vertices)
      (vector-push-extend (coerce r 'single-float) vertices)
      (vector-push-extend (coerce g 'single-float) vertices)
      (vector-push-extend (coerce b 'single-float) vertices)
      (vector-push-extend (coerce a 'single-float) vertices)
      (vector-push-extend index indices)
      (values idx0 index))))

(defun random-line-cloud (&optional (n 100))
  (let ((lines (make-line-segments)))
    (dotimes (i n)
      (add-line-2 lines
                  :p1 (vec3-random -0.98 0.98)
                  :p2 (vec3-random -0.98 0.98)
                  :c1 (vec4-random 0.2 0.8)
                  :c2 (vec4-random 0.2 0.8)))
    lines))


(defun make-square ()
  (let ((ls (make-line-segments)))
    (add-line ls
                    :x1 0.0 :y1 0.0 :z1 0.0
                    :x2 1.0 :y2 0.0 :z2 0.0)
    (add-line ls
                    :x1 1.0 :y1 0.0 :z1 0.0
                    :x2 1.0 :y2 1.0 :z2 0.0)
    (add-line ls
                     :x1 1.0 :y1 1.0 :z1 0.0
                     :x2 0.0 :y2 1.0 :z2 0.0)
    (add-line ls
                    :x1 0.0 :y1 1.0 :z1 0.0
                    :x2 0.0 :y2 0.0 :z2 0.0)
    ls))


(defun create-axis (size &key (color t)
                           (half nil))
  (let* ((axis (make-line-segments))
         x-color
         y-color
         z-color
         (min-value(if half 0 (- size)))
         (max-value size))
    (cond ((eq color :black)
           (setf x-color (vec4 0 0 0 1)
                 y-color (vec4 0 0 0 1)
                 z-color (vec4 0 0 0 1)))
          ((eq color :white)
           (setf x-color (vec4 1 1 1 1)
                 y-color (vec4 1 1 1 1)
                 z-color (vec4 1 1 1 1)))
          ((eq (type-of color) 'vec4)
           (setf x-color color
                 y-color color
                 z-color color))
          (t
           (setf x-color (vec4 1 0 0 1)
                 y-color (vec4 0 1 0 1)
                 z-color (vec4 0 0 1 1))))
    (add-line-2 axis
                :p1 (vec3 min-value 0 0) :c1 x-color
                :p2 (vec3 max-value 0 0) :c2 x-color)
    (add-line-2 axis
                :p1 (vec3 0 min-value 0) :c1 y-color
                :p2 (vec3 0 max-value 0) :c2 y-color)
    (add-line-2 axis
                :p1 (vec3 0 0 min-value) :c1 z-color
                :p2 (vec3 0 0 max-value) :c2 z-color)
    axis))

(defun read-lines-from-text-file (filename)
  (let ((ls (make-line-segments))
        (pts (with-input-from-file (inf filename)
               (loop for xc = (read inf nil nil)
                     for yc = (read inf nil nil)
                     while (and xc yc)
                     collect (vec3 (coerce xc 'double-float)
                                   0.0
                                   (coerce yc 'double-float)
                                   )))))
    (loop for first in pts
          for second in (cdr pts)

          do
          (add-line-2 ls
                      :p1 first
                      :c1 (vec4 0 1 0 1)
                      :p2 second
                      :c2 (vec4 0 1 0 1))

          maximizing (vx first) into max-x
          maximizing (vy first) into max-y
          maximizing (vz first) into max-z

          minimizing (vx first) into min-x
          minimizing (vy first) into min-y
          minimizing (vz first) into min-z

          finally (add-line-2 ls
                              :p1 second
                              :c1 (vec4 0 1 0 1)
                              :p2 (car pts)
                              :c2 (vec4 0 1 0 1))
          finally (return (values ls (vec3 min-x -1.0 min-z) (vec3 max-x 1.0 max-z))))))

(defun draw-line (container
                  pt-1
                 pt-2
                  color-1
                  &optional color-2)
  (add-line-2 container :p1 pt-1 :c1 color-1 :p2 pt-2 :c2 (if color-2 color-2 color-1)))


(defun draw-triangle (container pt-1 pt-2 pt-3 color)
  (add-line-2 container :p1 pt-1 :c1 color :p2 pt-2 :c2 color)
  (add-line-2 container :p1 pt-2 :c1 color :p2 pt-3 :c2 color)
  (add-line-2 container :p1 pt-3 :c1 color :p2 pt-1 :c2 color))
